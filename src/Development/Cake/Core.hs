{-# LANGUAGE DeriveDataTypeable, BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Development.Cake.Core where

import Control.Applicative
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception as Exception hiding ( unblock )
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.ParallelIO.Local
import Data.Binary
import Data.Either
import Data.List
import Data.Maybe ( catMaybes )
import Data.Typeable ( Typeable(..) )
import GHC.Conc ( numCapabilities )
import System.Directory ( getModificationTime )
import System.FilePath.Canonical
import System.IO.Error ( isDoesNotExistError )
import System.Time ( ClockTime(..) )
import qualified Data.Map as M

newtype ModTime = M ClockTime

instance Binary ModTime where
  put (M (TOD t1 t2)) = put t1 >> put t2
  get = do t1 <- get; t2 <- get; return (M (TOD t1 t2))

instance Eq ModTime where M t1 == M t2 = t1 == t2
instance Show ModTime where show (M t) = show t

newtype Database = DB { unDB :: M.Map CanonicalFilePath Status }
  deriving Show

printDatabase :: Database -> IO ()
printDatabase (DB db) = mapM_ print (M.toList db)

instance Binary Database where
  put (DB db) = put db
  get = DB <$> get

type Target = CanonicalFilePath

-- | A question with a namespace and a question as an arbitrary string.
--
-- The user usually shouldn't use this type.  Instead, the oracle provider
-- should implement wrappers that construct the question of the right form
-- and pass it to 'query'.
--
-- See also: 'installOracle'.
--
data Question = Question String String -- namespace + question
  deriving (Eq, Ord)

instance Show Question where
  show (Question namespace question) =
    "Q[" ++ namespace ++ "]: " ++ show question

instance Binary Question where
  put (Question n q) = put n >> put q
  get = Question <$> get <*> get

-- TODO: ATM, Oracles are expected to do their own answer-caching.

-- | Install a new oracle for answering non-file requirements.
--
-- Oracles are used for things such as listing directory contents or
-- querying environment variables.  It's called an oracle because all
-- it does is answer questions, but we don't care how it does that.
--
-- An oracle should typically be total.  The @Nothing@ result is intended
-- to communicate the fact that the given oracle didn't understand the
-- question.  Each question has a namespace, and an oracle should only
-- answer questions for its namespace.  Use exceptions if partiality is
-- really required, e.g., @ls@ may fail due to access control errors.
--
installOracle :: (Question -> IO (Maybe Answer)) -> Cake ()
installOracle fn = Cake $ \mst -> do
  modifyMVar_ mst $ \st ->
    return st{ csOracle = fn `combineOracle` csOracle st }
 where
   combineOracle oracle1 oracle2 question = do
     mb_ans <- oracle1 question
     case mb_ans of
       Nothing -> oracle2 question
       Just _  -> return mb_ans

query :: Question -> Act Answer
query question = do
  oracle <- aeOracle <$> askActEnv
  mb_answer <- liftIO (try (oracle question))
  case mb_answer of
    Right (Just answer) -> do
      appendHistory (Oracle question answer)
      return answer
    Right Nothing ->
      fail $ "Could not answer question: " ++ show question ++
             "\nMissing oracle?"
    Left err -> liftIO (throwIO (WrappedException err))

type Answer = [String]

data Status
  = Dirty History ModTime
  | Building WaitHandle
  | Clean History ModTime
  | Failed CakeException
  deriving Show

instance Binary Status where
  put (Dirty hist t) = putWord8 1 >> put hist >> put t
  put (Clean hist t) = putWord8 2 >> put hist >> put t
  put (Building _) = error "Cannot serialise in-progress database"
  put (Failed msg) = putWord8 3 >> put (showCakeException msg)

  get = do
    tag <- getWord8
    case tag of
      1 -> Dirty <$> get <*> get
      2 -> Dirty <$> get <*> get  -- load all files as dirty!
      3 -> Failed . RuleError <$> get
      _ -> error "Invalid tag when deserialising Status"

-- State transitions:
--
-- Dirty|not-in-DB -> Building -> Clean|Failed

type Reason = String

data ActEnv = ActEnv
  { aeDatabase :: MVar Database
  , aeRules :: [Rule]
  , aeOracle :: Question -> IO (Maybe Answer)
  , aePool :: Pool
  , aeLogLock :: MVar ()
  , aeNestLevel :: Int
    -- ^ Lock for printing messages.  Without the lock, single
    -- characters might be interleaved when using 'putStrLn'.  Use
    -- 'report' instead.
  }

newtype Verbosity = V Int

chatty :: Verbosity
chatty = V 4

silent :: Verbosity
silent = V 0

report :: Verbosity -> String -> Act ()
report verb msg = do
  env <- askActEnv
  liftIO $ report' env verb msg

report' :: ActEnv -> Verbosity -> String -> IO ()
report' env _verb msg =
  withMVar (aeLogLock env) $ \() -> do
    let indentStr = replicate (2 * (aeNestLevel env)) ' '
    let msg_lines = unlines (map (indentStr++) (lines msg))
    putStr msg_lines

-- | Rules within a rule set are in the same order in which they were
-- added.
type RuleSet = [Rule]

newtype History = H [QA]
  deriving Show

instance Binary History where
  put (H qas) = put qas
  get = H <$> get

-- | An entry in the history.
-- 
-- For files, it generates 
data QA = Oracle Question Answer
        | Need [(CanonicalFilePath, ModTime)]
 deriving Show

instance Binary QA where
  put (Need entries) = putWord8 1 >> put entries
  put (Oracle question ans) = putWord8 2 >> put question >> put ans
  get = do
    tag <- getWord8
    case tag of
      1 -> Need <$> get
      2 -> Oracle <$> get <*> get
      _ -> error "Cannot decode QA"

instance Binary CanonicalFilePath where
  put cfp = put (originalFilePath cfp) >> put (canonicalFilePath cfp)
  get = unsafeCanonicalise <$> get <*> get

newtype Act a = Act { unAct :: ActEnv -> MVar ActState -> IO a }

askActEnv :: Act ActEnv
askActEnv = Act (\env _ -> return env)

tryAct :: Exception e => Act a -> Act (Either e a)
tryAct body = Act (\env mst -> try (unAct body env mst))

instance Functor Act where 
  fmap f act = Act (\env mst -> fmap f (unAct act env mst))

instance Monad Act where
  return x = Act (\_env _mst -> return x)
  act >>= k = Act (\env mst -> do
                a <- unAct act env mst
                unAct (k a) env mst)
  fail msg = Act (\_env _mst -> throwIO (RuleError msg))

instance MonadIO Act where
  liftIO ioact = Act (\_env _mst -> ioact)

data ActState = ActState
  { asHistory :: History }

appendHistory :: QA -> Act ()
appendHistory qa = Act (\_env mst ->
  modifyMVar_ mst $ \st ->
    let H hist = asHistory st in
    return st{ asHistory = H (hist ++ [qa]) })

newtype Cake a = Cake { unCake :: MVar CakeState -> IO a }

data CakeState = CakeState
  { csRules :: [Rule],
    csActs :: [Act ()],
    csOracle :: Question -> IO (Maybe Answer)
  }

instance Monad Cake where
  return x = Cake (\_mst -> return x)
  act >>= k = Cake (\mst -> do
                      a <- unCake act mst
                      unCake (k a) mst)

instance MonadIO Cake where
  liftIO act = Cake (\_mst -> act)

-- | Queue an action to be run when all rules have been loaded.
-- 
-- There is no guarantee about the order in which items scheduled by
-- multiple calls to this function are executed.  I.e., if we have
-- 
-- > queueAct act1 >> queueAct act2
-- 
-- then @act1@ and @act2@ will be executed in any order or in
-- parallel.
queueAct :: Act () -> Cake ()
queueAct act = Cake $ \mst ->
  modifyMVar_ mst $ \st ->
    return st{ csActs = act : csActs st }
  

type Rule = CanonicalFilePath -> IO (Maybe Generates)

-- | Specification of a rule result.
data Generates = Generates
  { genOutputs :: [CanonicalFilePath]
    -- ^ The files that the rule generates.
  , genAction  :: Act [ModTime]
    -- ^ The action to generate the specified files.  Returns
    -- modification times of the generated files (in the same order).
  }

mkDatabase :: IO (MVar Database)
mkDatabase = newMVar (DB M.empty)

type Pattern = String

data CakeException
  = RuleError String
  | RecursiveError [(TargetDescr, CakeException)]
  | WrappedException SomeException
  deriving Typeable

type TargetDescr = String

instance Show CakeException where
  show = unlines . showCakeException

showCakeException :: CakeException -> [String]
showCakeException (RuleError s) =
  ["Error in rule definition: " ++ s]
showCakeException (WrappedException e) =
  ["Error while executing rule: " ++ show e]
showCakeException (RecursiveError nested_errs) =
  ["The following dependenc" ++ plural_y ++ " failed to build: "] ++
  map indent (concatMap showNested nested_errs)
 where
   indent line = "  " ++ line
   showNested (target, exception) =
     [ target ++ ":" ] ++ map indent (showCakeException exception)
   plural_y = case nested_errs of
                [_] -> "y"
                _ -> "ies"

instance Exception.Exception CakeException

instance NFData CakeException where
    rnf (RuleError a) = rnf a
    rnf (RecursiveError nesteds) = rnf nesteds
    rnf (WrappedException _) = ()

type CakeSuccess a = Either CakeException a

cakeError :: String -> IO a
cakeError s = Exception.throwIO $ RuleError s

panic :: String -> IO a
panic s = Exception.throwIO $ RuleError ("PANIC: " ++ s)

-- | Find a single rule matching the given file.
-- 
-- Returns:
-- 
--  * The targets that would be produced by the matched rule.
-- 
--  * The action to run in order to produce these targets.  The returned
--    modification times are in the same order as the list of targets.
-- 
-- This is in 'IO' because looking up a rule may fail (in which case
-- an exception will be thrown).
findRule :: ActEnv -> RuleSet -> CanonicalFilePath
         -> IO ([CanonicalFilePath], ActEnv -> IO (History, [ModTime]))
findRule env ruleSet goal = do
  mb_gens <- mapMaybeM (tryToMatch goal) ruleSet
  case mb_gens of
    [] -> do
      report' env chatty $
        "NORULE " ++ show goal ++ ": using default rule"
      mb_dflt <- defaultRule env goal
      case mb_dflt of
        Nothing -> cakeError $ "No rule to build " ++ show goal
        Just (outs, act) ->
          return (outs, \_env -> do modtimes <- liftIO act
                                    return (H [], modtimes))
    [gen] -> do
      report' env chatty $
        "RULE " ++ show goal ++ ": outputs: " ++ show (genOutputs gen)
      return (genOutputs gen, \e -> runAct e (genAction gen))
    (gen:_) -> do
      report' env silent $
        "Ambiguous rules for " ++ show goal ++
        ": choosing first one."
      return (genOutputs gen, \e -> runAct e (genAction gen))
 where
   tryToMatch gl rule = rule gl

-- | Returns the default rule for the given target (if any).
-- 
-- For a file we have a default rule if the file already exists on
-- disk.  The returned action merely returns the modification date of
-- the file.
defaultRule :: ActEnv -> CanonicalFilePath
            -> IO (Maybe ([CanonicalFilePath], IO [ModTime]))
defaultRule env file = do
  mb_time <- getFileModTime file
  case mb_time of
    Nothing -> return Nothing
    Just modtime ->
        return (Just ([file],
                      do report' env chatty $
                           "MODTIME: " ++ show file ++ " = " ++ show modtime
                         return [modtime]))

-- | Monadic map over a list keeping the @Just x@ results of the function.
-- 
-- In other words, @mapMaybeM = liftM catMaybes . mapM@ but it's slightly
-- more efficient.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = go
  where go []     = return []
        go (x:xs) = do
          mb_y <- f x
          case mb_y of
            Nothing ->             go xs
            Just y  -> liftM (y:) (go xs)

-- | Run an @Act a@ action with an initially empty history and return
-- final history.
runAct :: ActEnv -> Act a -> IO (History, a)
runAct env (Act act) = do
  mst <- newMVar (ActState{ asHistory = H [] })
  res <- act env mst
  st <- readMVar mst
  return (asHistory st, res)

-- Check whether the given targets are up to date.
-- 

type NeedsRebuilding = Either CakeException [ModTime]

-- | The result of checking a history entry.
data CheckQAResult
  = QAFailure CakeException
    -- ^ The target (or any of its dependencies) failed to build.
  | QAClean
    -- ^ The target (and all its dependencies) is up to date.
  | QARebuild
    -- ^ The target needs to be rebuilt.

-- | Check a single history entry.
-- 
-- Checking a single dependency means also checking its dependencies
-- and their dependencies, etc.  If any of the dependencies need
-- rebuilding, we do so right away.  In these cases, the checked
-- target needs rebuilding as well, so we return 'QARebuild' (or
-- 'QAFailure').
checkQA :: ActEnv -> QA -> IO CheckQAResult
checkQA env (Need entries) = do
  let (outputs, old_modtimes) = unzip entries
  result <- need' env outputs
  case result of
    Right new_modtimes -> do
      let oks = zipWith3 check outputs new_modtimes old_modtimes
      report' env chatty $
        "CHECKQA " ++ show (zip new_modtimes old_modtimes) ++ "\n" ++
        show oks
      return (if and oks then QAClean else QARebuild)
    Left err -> return (QAFailure err)
 where
   check _goal new_time old_time = old_time == new_time

checkQA env (Oracle question old_answer) = do
  mb_new_answer <- try (aeOracle env question)
  report' env chatty $
    "CHECKQA " ++ show (question, old_answer, mb_new_answer)
  case mb_new_answer of
    Right Nothing ->
      -- Weird, the current oracle couldn't answer the old question.
      -- Perhaps we updated the build file, so let's try to just
      -- rebuild.  If we ask the same question again we'll get the
      -- error during rebuilding.
      return QARebuild

    Right (Just new_answer) -> 
      if new_answer /= old_answer then return QARebuild else return QAClean

    Left err ->
      return (QAFailure (WrappedException err))

-- TODO: Example: We question the environment and a variable is no
-- longer defined

checkHistory :: ActEnv -> History -> IO CheckQAResult
checkHistory env (H hist) = do 
  report' env chatty $ "HISTCHECK (" ++ show (length hist) ++ ")"
  go hist
 where
   go []       = return QAClean
   go (qa:qas) = do rebuild <- checkQA env qa
                    case rebuild of
                      QAClean -> go qas
                      _ -> return rebuild

checkOne :: ActEnv -> CanonicalFilePath
         -> IO (Either CakeException ModTime)
checkOne env goal_ = do
  todo <- grabTodo goal_
  result <- 
    case todo of
      UptoDate modtime -> do
        report' env chatty $ "CLEAN " ++ show goal_
        return (Right modtime)

      BlockOn waitHandle -> do
        -- We are about to block on the wait handle, make sure we
        -- release the worker as required by "parallel-io"
        report' env chatty $ "BLOCK " ++ show goal_
        extraWorkerWhileBlocked (aePool env) $ do
          result <- waitOnWaitHandle waitHandle
          report' env chatty $ "UNBLOCK " ++ show goal_
          return result

      CheckHistory _hist _modtime unblock targets Nothing action -> do
        -- One of the targets produced by the action is already known
        -- to require rebuilding.  We can shortcut this case here.
        let reason = "One of its rule targets needs rebuilding"
        runRule reason unblock targets action

      CheckHistory hist modtime unblock targets (Just modtimes) action -> do
        -- TODO: Do we want to check the history of all 'targets'?
        -- This rule creates all our targets.  Nevertheless, some
        -- targets may have different modtimes.  However, we're only
        -- checking one file and we can assume that all targets share
        -- the same history (really?).  Rules that conditionally
        -- create targets are forbidden.
        --
        -- Note: Checking history involves recursively
        -- checking/building all dependencies first.
        --
        -- Should we check history of all rule outputs?  No - they
        -- must have the same history!  (Unless we have overlapping
        -- rules.)
        needs_rebuild <- checkHistory env hist
        case needs_rebuild of
          QARebuild ->
            runRule "history check failed, one or more dependencies changed"
                    unblock targets action
          QAFailure err -> do
            modifyMVar_ (aeDatabase env) $ \db ->
              return $ updateStatus db (targets `zip` (repeat (Failed err)))
            _ <- unblock (Left err)
            return (Left err)
          QAClean -> do
            -- The history might be clean, but the file has been
            -- modified.  This could be the case if it's a file on disk
            -- (which doesn't have any dependencies) or it's an
            -- auto-generated file and the user has accidentally edited
            -- it.  In that case we must rebuild the file.
            mb_time <- getFileModTime goal_
            case mb_time of
              Nothing -> do
                -- History is clean, but file doesn't exist?  May have
                -- been deleted in a clean action.  Just rebuild.
                runRule "history clean, but file does not exist"
                        unblock targets action
              Just newtime | newtime /= modtime -> do
                report' env chatty $ "MODIFIED " ++ show goal_ ++
                                     show (modtime, newtime)
                runRule "history clean, but file has been modified"
                        unblock targets action
              _ -> do
                -- NOW the target is actually clean.
                report' env chatty $
                  "HISTCLEAN " ++ show goal_ ++ " No rebuild needed"
                modifyMVar_ (aeDatabase env) (\db -> do
                  let db' = updateStatus db $
                              zip targets (map (Clean hist) modtimes)
                  return db')
                unblock (Right modtimes)
                return (Right modtime)

      Rebuild reason unblock targets _modtimes action -> do
        runRule reason unblock targets action

      PropagateFailure exc -> do
        return (Left exc)

  report' env chatty $ "DONE " ++ show goal_ ++ " " ++ show result
  return result
 where
   -- Atomically grab a BuildTodo.
   --
   -- If the Todo requires action, we immediately change the current
   -- state of the item in the database to Building, so that other
   -- workers will wait for us.
   grabTodo :: CanonicalFilePath -> IO BuildTodo
   grabTodo goal = do
     modifyMVar (aeDatabase env) $ \db@(DB mdb) ->
       case M.lookup goal mdb of
         Nothing -> do
           markItemAsBuilding goal db (Rebuild "Not in database")
         Just (Dirty hist modtime) ->
           markItemAsBuilding goal db (CheckHistory hist modtime)
         Just (Clean _hist modtime) ->
           return (db, UptoDate modtime)
         Just (Building waitHandle) ->
           return (db, BlockOn waitHandle)
         Just (Failed exception) ->
           return (db, PropagateFailure exception)
--           markItemAsFailed goal db exception
           -- TODO: mark all rule targets as failed, too.
           --return (db, PropagateFailure exception)
--           panic "NYE: Something failed"

   -- When we discover that an item *may* need rebuilding, we have to
   -- lock it in the database before we release the lock.
   
   markItemAsBuilding :: CanonicalFilePath -> Database
                      -> ((Either CakeException [ModTime] -> IO ())
                          -> [CanonicalFilePath] 
                          -> Maybe [ModTime]
                          -> (ActEnv -> IO (History, [ModTime]))
                          -> BuildTodo)
                      -> IO (Database, BuildTodo)
   markItemAsBuilding goal db todo_kont = do
     -- TODO: Sanity check (goal `member` outputs)
     -- TODO: Sanity check: none of the other outputs are already building
     -- TODO: findRule may throw an exception!
     mb_rule <- try $ findRule env (aeRules env) goal
     case mb_rule of
       Right (outputs, action) -> do
         (unblock, waitHandles) <- newWaitHandle outputs
         -- We need to lock all possibly generated targets at once.
         let db' = updateStatus db (zip outputs (map Building waitHandles))
         let mtimes = targetModTimes db outputs
         return (db', todo_kont unblock outputs mtimes action)
       Left exc -> do
         let db' = updateStatus db (zip [goal] [Failed exc])
         return (db', PropagateFailure exc)
--         return (db', 

   runRule rebuildReason unblock targets action = do
     report' env chatty $
       "REBUILD " ++ show goal_ ++ ": " ++ rebuildReason
     -- Execute the action
     actResult <- try (action env)
     case actResult of
       Right (hist, modtimes) -> do
         modifyMVar_ (aeDatabase env) $ \db ->
           return $ updateStatus db (targets `zip` map (Clean hist) modtimes)
         _ <- unblock (Right modtimes)
         let Just idx = elemIndex goal_ targets
          -- return modification date of current goal
         return (Right (modtimes !! idx))
       Left (someExc :: SomeException) -> do
         -- Yes, we're catching *all* exceptions.  This should be
         -- fine, because we're going to abort building anyway.  We
         -- only unroll the dependency chain to build a more
         -- informative error.
         --
         -- The final top-level 'need' will then rethrow the resulting
         -- exception.  A problem is that the top-level exception will
         -- always be a 'CakeException', if the user pressed
         -- Control-C that information will be hidden inside the
         -- CakeException.
         let exc = wrapException someExc
         modifyMVar_ (aeDatabase env) $ \db ->
           return $ updateStatus db (targets `zip` (repeat (Failed exc)))
         _ <- unblock (Left exc)
         return (Left exc)

wrapException :: SomeException -> CakeException
wrapException exc =
  case fromException exc of
    Just e -> e
    Nothing -> WrappedException exc

-- | Updates each 'Target''s status in the database.
updateStatus :: Database -> [(Target, Status)] -> Database
updateStatus db      []                 = db
updateStatus (DB db) ((goal, st):goals) =
  let !db' = M.insert goal st db in
  updateStatus (DB db') goals

-- | Find modification times for targets if available for all targets.
-- 
-- Returns @Just modtimes@ iff a modification date was available for
-- each of the targets, i.e., for each file we either have a history
-- or have already built it.  @Nothing@, otherwise.
targetModTimes :: Database -> [Target] -> Maybe [ModTime]
targetModTimes (DB db) targets = go targets []
 where
   go [] modtimes = Just (reverse modtimes)
   go (tgt:tgts) modtimes =
     case M.lookup tgt db of
       Just (Dirty _hist modtime) -> go tgts (modtime:modtimes)
       Just (Clean _hist modtime) -> go tgts (modtime:modtimes)
       _ -> Nothing

-- The 'Building' race:
--
-- We concurrently check targets for some to rebuild.  We want to
-- ensure that each target is only built by one thread.  If a worker
-- decides to build a target, the worker must lock it in the database.
--
-- Some rules may create multiple targets, say [A, B].  Now consider
-- the case where two workers concurrently check A and B.  If one
-- worker decides to build A it cannot just lock A, it must lock B,
-- too.  This in turn requires that we find the rule to build A while
-- holding the database lock.
--
-- This means that we need to look up the rule, even if we just need
-- to check the history, because we have to lock all files while
-- checking the history.

-- | A wait handle.
-- 
-- If we discover that a file is building, we store a @WaitHandle@ in
-- the database.  If another worker thread comes along it will block
-- on the wait handle.  When the building thread is done, it will
-- unblock the wait handle and wake up any waiting threads.
-- 
-- A rule may build multiple targets.  This means that multiple
-- threads may wait on the same action to finish, but actually want to
-- read different files.  We therefore create multiple wait handles
-- (one for each output) but they all share the same internal @MVar@.
-- 
-- When a wait handle is unblocked we return the modification time
-- of the corresponding target.  We therefore store a selector function
-- to pick the modification time.
data WaitHandle =
  WaitHandle (MVar [BuildResult]) ([BuildResult] -> BuildResult)

type BuildResult = Either CakeException ModTime

instance Show WaitHandle where show _ = "<waithandle>"

-- | Create a new 'WaitHandle' for the given targets.
-- 
-- This returns:
-- 
--  * A function to unblock the handle.  It takes as argument the
--    modification times of the targets.  Note that the order of the
--    modification times must match the order of the targets.  I.e.,
--    the first modification time, must be the modification time of
--    the first target and so on.
-- 
--  * multiple handles that get unblocked simultaneously, but may
--    return different 'ModTime's.  The order matches the order of the
--    targets.
-- 
newWaitHandle :: [CanonicalFilePath]
              -> IO (Either CakeException [ModTime] -> IO (),
                     [WaitHandle])
newWaitHandle goals = do
  mvar <- newEmptyMVar
  let ngoals = length goals
  return (\modtimes ->
            case modtimes of
              Left exc -> putMVar mvar (replicate ngoals (Left exc))
              Right mtimes -> putMVar mvar (map Right mtimes),
          map (WaitHandle mvar) (take (length goals) selectorFunctions))

-- | Block on a @WaitHandle@ until it's done.
waitOnWaitHandle :: WaitHandle -> IO BuildResult
waitOnWaitHandle (WaitHandle mvar f) = do
  modtimes <- readMVar mvar
  return (f modtimes)

-- | The infinite list of element selector functions.  That is:
-- 
-- > [(!! 0), (!! 1), (!! 2), ...]
selectorFunctions :: [[a] -> a]
selectorFunctions = map nth [0..]
  where nth n xs = xs !! n

-- | Describes what we should do with the target.
data BuildTodo
  = Rebuild Reason -- why are we rebuilding?
            UnblockAction -- unblock function of wait handle.
            [CanonicalFilePath] -- the targets we're goint to build
            (Maybe [ModTime]) -- their modification times if all known
            (ActEnv -> IO (History, [ModTime])) -- build action
    -- ^ The target must be rebuilt.  The 'Reason' is a human-readable
    -- description of why it needs to be rebuilt.
  | CheckHistory History ModTime
                 UnblockAction  -- same as above
                 [CanonicalFilePath]
                 (Maybe [ModTime])
                 (ActEnv -> IO (History, [ModTime]))
    -- ^ The target may not be up to date, so we have to check its
    -- history.
  | UptoDate ModTime
    -- ^ The target is up to date.  Nothing needs to be done.
  | BlockOn WaitHandle
    -- ^ The target is currently being processed by another worker,
    -- (or that worker is blocked on another dependency and so on) so
    -- we have to wait for it.
  | PropagateFailure CakeException
    -- ^ We previously tried to build the target and got an error.  We
    -- now propagate this error to its dependents.

type UnblockAction = Either CakeException [ModTime] -> IO ()

------------------------------------------------------------------------
-- * User API

-- | Ensure all given targets are up to date.
need' :: MonadIO m =>
         ActEnv -> [CanonicalFilePath]
      -> m (Either CakeException [ModTime])
need' env goals = do
  results <- liftIO $ parallel (aePool env) $
               map (checkOne env{ aeNestLevel = aeNestLevel env + 1 }) goals
  case partitionEithers results of
    ([], modtimes) -> return (Right modtimes)
    (_:_, _) ->
      return (Left (mkRecursiveError (zip goals results)))

mkRecursiveError :: [(CanonicalFilePath, Either CakeException ModTime)]
                 -> CakeException
mkRecursiveError = RecursiveError . mapFilter keepError
 where
   keepError (fp, Left err) = Just (show fp, err)
   keepError _              = Nothing

-- | Combines 'map' and 'filter' into one operation.  If the function
-- argument returns @Just y@, then @y@ will appear in the result list.
-- Preserves ordering of the input list.
mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f = catMaybes . map f

need :: [CanonicalFilePath] -> Act [ModTime]
need goals = do
  env <- askActEnv
  result <- need' env goals
  case result of
    Right modtimes -> do
      appendHistory (Need (goals `zip` modtimes))
      return modtimes
    Left err ->
      liftIO $ throwIO err

-- | Run a 'Cake' monad.
cake :: Cake () -> IO ()
cake collectRules = do
  mst <- newMVar (CakeState{ csRules = []
                           , csActs = []
                           , csOracle = \_ -> return Nothing })
  unCake collectRules mst

  st <- takeMVar mst
  let rules = reverse (csRules st)
      acts = reverse (csActs st)
      oracle = csOracle st
  db <- loadDatabase databaseFilePath
  printDatabase db
  mdb <- newMVar db
  logLock <- newMVar ()
  -- | TODO: Make customisable
  let poolSize = numCapabilities + 1
  withPool poolSize $ \pool -> do
    let env = ActEnv{ aeDatabase = mdb,
                      aeRules = reverse rules,
                      aeOracle = oracle,
                      aePool = pool,
                      aeLogLock = logLock,
                      aeNestLevel = 0 }
    -- This is where we kick off the actual work
    parallel_ pool $ map (runAct env) acts
    report' env chatty $ "Writing database"
    db' <- takeMVar mdb
    printDatabase db'
    writeDatabase databaseFilePath db'
  return ()

addRule :: Rule -> Cake ()
addRule rule = Cake (\mst -> do
  modifyMVar_ mst (\st ->
    return st{ csRules = rule : csRules st }))

databaseFilePath :: FilePath
databaseFilePath = ".cake-db"

loadDatabase :: FilePath -> IO Database
loadDatabase fp =
  stripFailures <$> handleDoesNotExist (return (DB M.empty)) (decodeFile fp)
 where
   -- TODO: We need to think about what should happen to on-disk DB
   -- entries for Failed builds.  Currently, we strip them from the
   -- loaded DB, so that we try to build them again this time around.
   -- Is there anything better we can do?
   stripFailures (DB mp) = DB (M.filter (not . isFailure) mp)

   isFailure (Failed _) = True
   isFailure _ = False

writeDatabase :: FilePath -> Database -> IO ()
writeDatabase = encodeFile

-- | Get file modification time (if the file exists).
getFileModTime :: CanonicalFilePath -> IO (Maybe ModTime)
getFileModTime cfp =
  handleDoesNotExist (return Nothing) $
    Just . M <$> getModificationTime (canonicalFilePath cfp)

-- | Do an action in case we get a file-does-not-exist exception.
-- 
-- Argument order is the same as for 'handle'.
handleDoesNotExist :: IO a  -- ^ Do this in case of error
                   -> IO a  -- ^ Try doing this.
                   -> IO a
handleDoesNotExist = handleIf isDoesNotExistError

-- | Variant of 'handleJust' with a predicate instead of a 'Just b'
-- transformer.
handleIf :: Exception.Exception e =>
            (e -> Bool) -- ^ Handle exception if this function returns 'True'.
         -> IO a -- ^ Handler (executed if body raises an exception).
         -> IO a -- ^ Body.
         -> IO a
handleIf p handler act =
  Exception.handleJust (guard . p) (\() -> handler) act

