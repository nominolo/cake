{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
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
import Data.List
import Data.Typeable ( Typeable(..) )
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
--   QA String
--  deriving (Eq, Ord, Show)

data Status
  = Dirty History ModTime
  | Building WaitHandle
  | Clean History ModTime
  | Failed Reason
  deriving Show

instance Binary Status where
  put (Dirty hist t) = putWord8 1 >> put hist >> put t
  put (Clean hist t) = putWord8 2 >> put hist >> put t
  put (Building _) = error "Cannot serialise in-progress database"
  put (Failed msg) = putWord8 3 >> put msg

  get = do
    tag <- getWord8
    case tag of
      1 -> Dirty <$> get <*> get
      2 -> Dirty <$> get <*> get  -- load all files as dirty!
      3 -> Failed <$> get
      _ -> error "Invalid tag when deserialising Status"

-- State transitions:
--
-- Dirty|not-in-DB -> Building -> Clean|Failed

type Reason = String

data ActEnv = ActEnv
  { aeDatabase :: MVar Database
  , aeRules :: [Rule]
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
data Question = Q String String
newtype Answer = A [String]
newtype History = H [QA]
  deriving Show

instance Binary History where
  put (H qas) = put qas
  get = H <$> get

-- | An entry in the history.
-- 
-- For files, it generates 
data QA = -- Oracle Question Answer
        Need [(CanonicalFilePath, ModTime)]
 deriving Show

instance Binary QA where
  put (Need entries) = putWord8 1 >> put entries
  get = do
    tag <- getWord8
    case tag of
      1 -> Need <$> get
      _ -> error "Cannot decode QA"

instance Binary CanonicalFilePath where
  put cfp = put (originalFilePath cfp) >> put (canonicalFilePath cfp)
  get = unsafeCanonicalise <$> get <*> get

newtype Act a = Act { unAct :: ActEnv -> MVar ActState -> IO a }

askActEnv :: Act ActEnv
askActEnv = Act (\env _ -> return env)

instance Monad Act where
  return x = Act (\_env _mst -> return x)
  act >>= k = Act (\env mst -> do
                a <- unAct act env mst
                unAct (k a) env mst)

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
    csActs :: [Act ()]
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

data CakefileException
  = RuleError String
  | RecursiveError [([String], CakefileException)]
  deriving Typeable

instance Show CakefileException where
  show = unlines . showCakefileException

showCakefileException :: CakefileException -> [String]
showCakefileException (RuleError s) =
  ["Error in rule definition: " ++ s]
showCakefileException (RecursiveError _) =
  ["Error in recursive invokation"]

instance Exception.Exception CakefileException

instance NFData CakefileException where
    rnf (RuleError a) = rnf a
    rnf (RecursiveError ls) = rnf ls

type CakeSuccess a = Either CakefileException a

cakefileError :: String -> IO a
cakefileError s = Exception.throwIO $ RuleError s

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
        Nothing -> cakefileError $ "No rule to build " ++ show goal
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

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = go
  where go []     = return []
        go (x:xs) = do
          mb_y <- f x
          case mb_y of
            Nothing ->             go xs
            Just y  -> liftM (y:) (go xs)

runAct :: ActEnv -> Act a -> IO (History, a)
runAct env (Act act) = do
  mst <- newMVar (ActState{ asHistory = H [] })
  res <- act env mst
  st <- readMVar mst
  return (asHistory st, res)

-- Check whether the given targets are up to date.
-- 
-- TODO: There are two ways to handle this:
-- 
--  1. Maintain a global work queue and workers just repeatedly
--     grab things from the work queue until everything is done.
-- 
--     This may have problems conceptially where a worker becomes
--     blocked on one task (e.g., it's waiting for a dependency to
--     build).  In this case we want the worker to start working on
--     something else.  There are two options:
-- 
--      a. The thread blocks, but no longer counts as a worker (by
--         releasing a lock).  Once the thread becomes unblocked
--         it tries to become a worker again.
-- 
--      b. We grab the current continuation and store it in the
--         database.  The worker is then free to to pick another
--         task.  Unless an error occurs, some worker will
--         eventually come back and pick up the continuation.
-- 
--     Option (a) is simpler, because in Option (b) we would have to
--     explicitly check that all dependencies are ready.  In Option
--     (a) we just block on all our dependencies and automatically get
--     woken up once the dependencies are ready.  In short, the
--     Haskell runtime system does the continuation management for us.
-- 
--  2. Build all dependencies in parallel.  Combined with Option (a)
--     above, this would mean we could hand off the work queue
--     managment to the Haskell thread scheduler as well.
-- 

type NeedsRebuilding = Bool

-- | Check a single history entry.
-- 
-- Returns ... (TODO)
checkQA :: ActEnv -> QA -> IO NeedsRebuilding
checkQA env (Need entries) = do
  let (outputs, old_modtimes) = unzip entries
  -- TODO: Exception handling?
  new_modtimes <- parallel (aePool env) $
                    map (checkOne env{ aeNestLevel = aeNestLevel env + 1 }) outputs
  let oks = zipWith3 check outputs new_modtimes old_modtimes
  report' env chatty $
    "CHECKQA " ++ show (zip new_modtimes old_modtimes) ++ "\n" ++
    show oks
  return (not (and oks))
 where
   check _goal new_time old_time = old_time == new_time

checkHistory :: ActEnv -> History -> IO NeedsRebuilding
checkHistory env (H hist) = do 
  report' env chatty $ "HISTCHECK (" ++ show (length hist) ++ ")"
  go hist
 where
   go []       = return False
   go (qa:qas) = do rebuild <- checkQA env qa
                    if rebuild then return True else go qas

checkOne :: ActEnv -> CanonicalFilePath -> IO ModTime
checkOne env goal_ = do
  todo <- grabTodo goal_
  modtime <- 
    case todo of
      UptoDate modtime -> do
        report' env chatty $ "CLEAN " ++ show goal_
        return modtime

      BlockOn waitHandle -> do
        -- We are about to block on the wait handle, make sure we
        -- release the worker as required by "parallel-io"
        report' env chatty $ "BLOCK " ++ show goal_
        extraWorkerWhileBlocked (aePool env) $ do
          mtime <- waitOnWaitHandle waitHandle
          report' env chatty $ "UNBLOCK " ++ show goal_
          return mtime

      CheckHistory _hist _modtime unblock targets Nothing action -> do
        -- One of the targets produced by the action is already known
        -- to require rebuilding.  We can shortcut this case here.
        let reason = "One of its rule targets needs rebuilding"
        runRule reason unblock targets action

      CheckHistory hist modtime unblock targets (Just modtimes) action -> do
        -- TODO: Do we want to check all 'targets'?  This rule creates all
        -- our targets.  Nevertheless, some targets may have different
        -- modtimes.  However, we're only checking one file and we 
        -- can assume that all targets share the same history (really?).
        -- What about a rule that conditionally creates additional targets?
        --
        -- Note: Checking history involves processing all recursively
        -- checking/building all dependencies first.
        --
        -- Should we check history of all rule outputs?  No - they
        -- must have the same history!
        needs_rebuild <- checkHistory env hist
        if not needs_rebuild then do
          -- The history might be clean, but the file has been
          -- modified.  This could be the case if it's a file on disk
          -- (which doesn't have any dependencies) or it's an
          -- auto-generated file and the user has accidentally edited.
          -- In that case we must rebuild the file.
          mb_time <- getFileModTime goal_
          case mb_time of
            Nothing -> do
              -- History is clean, but file doesn't exist?  That's weird!
              -- Just rebuild.
              runRule "history clean, but file does not exist"
                      unblock targets action
            Just newtime | newtime /= modtime -> do
              report' env chatty $ "MODIFIED " ++ show goal_ ++
                                   show (modtime, newtime)
              runRule "history clean, but file has been modified"
                      unblock targets action
            _ -> do
              -- NOW it's actually clean
              report' env chatty $
                "HISTCLEAN " ++ show goal_ ++ " No rebuild needed"
              modifyMVar_ (aeDatabase env) (\db -> do
                -- TODO: These should be the modtimes corresponding to the
                -- targets (they may be slightly different)
                let db' = updateStatus db $ zip targets (map (Clean hist) modtimes)
                return db')
              unblock modtimes
              return modtime
         else do
           runRule "history check failed, one or more dependencies changed"
                   unblock targets action

      Rebuild reason unblock targets _modtimes action -> do
        runRule reason unblock targets action
  report' env chatty $ "DONE " ++ show goal_ ++ " " ++ show modtime
  return modtime
 where
   -- Atomically grab a BuildTodo.
   --
   -- If the Todo requires action, we immediately change the current
   -- state of the item in the database, so that other workers will
   -- wait for us.
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
         Just (Failed _reason) ->
           panic "NYE: Something failed"

   -- When we discover that an item *may* need rebuilding, we have to
   -- lock it in the database before we release the lock.

   markItemAsBuilding goal db todo_kont = do
     -- TODO: Sanity check (goal `member` outputs)
     -- TODO: Sanity check: none of the outputs are already building
     (outputs, action) <- findRule env (aeRules env) goal
     (unblock, waitHandles) <- newWaitHandle outputs
     -- We need to lock all possibly generated targets at once.
     let db' = updateStatus db (zip outputs (map Building waitHandles))
     let mtimes = targetModTimes db outputs
     return (db', todo_kont unblock outputs mtimes action)

   runRule rebuildReason unblock targets action = do
     report' env chatty $
       "REBUILD " ++ show goal_ ++ ": " ++ rebuildReason
     -- Execute the action
     (hist, modtimes) <- action env
     modifyMVar_ (aeDatabase env) $ \db ->
       return $ updateStatus db (targets `zip` map (Clean hist) modtimes)
     _ <- unblock modtimes
     let Just idx = elemIndex goal_ targets
      -- return modification date of current goal
     return (modtimes !! idx)

-- | Updates each 'Target''s status in the database.
updateStatus :: Database -> [(Target, Status)] -> Database
updateStatus db      []                 = db
updateStatus (DB db) ((goal, st):goals) =
  let !db' = M.insert goal st db in
  updateStatus (DB db') goals

-- | Find modification times for targets if available for all targets.
-- 
-- Returns @Just modtimes@ iff a modification date was available for
-- each targets, i.e., for each file we either have a history or have
-- already built it.  @Nothing@, otherwise.
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

--data WaitHandle = 

data WaitHandle = WaitHandle (MVar [ModTime]) ([ModTime] -> ModTime)

instance Show WaitHandle where show _ = "<waithandle>"

-- | Create a new 'WaitHandle' for the given targets.
-- 
-- This returns:
-- 
--  * the function to unblock the handle
-- 
--  * multiple handles that get unblocked simultaneously, but may
--    return different 'ModTime's.
-- 
newWaitHandle :: [CanonicalFilePath]
              -> IO ([ModTime] -> IO (), [WaitHandle])
newWaitHandle goals = do
  mvar <- newEmptyMVar
  return (\modtimes -> putMVar mvar modtimes
         ,map (WaitHandle mvar) (take (length goals) selectorFunctions))

waitOnWaitHandle :: WaitHandle -> IO ModTime
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
  = Rebuild Reason ([ModTime] -> IO ())
            [CanonicalFilePath] (Maybe [ModTime])
            (ActEnv -> IO (History, [ModTime]))
    -- ^ The target must be rebuilt.  The 'Reason' is a human-readable
    -- description of why it needs to be rebuilt.
  | CheckHistory History ModTime ([ModTime] -> IO ())
                 [CanonicalFilePath] (Maybe [ModTime])
                 (ActEnv -> IO (History, [ModTime]))
    -- ^ The target may not be up to date, so we have to check its
    -- history.
  | UptoDate ModTime
    -- ^ The target is up to date.  Nothing needs to be done.
  | BlockOn WaitHandle
    -- ^ The target is currently being processed by another worker,
    -- (or that worker is blocked on another dependency and so on) so
    -- we have to wait for it.

------------------------------------------------------------------------
-- * User API

need :: [CanonicalFilePath] -> Act [ModTime]
need goals = do
  env <- askActEnv
  modtimes <- liftIO $ parallel (aePool env) $ map (checkOne env{ aeNestLevel = aeNestLevel env + 1 }) goals
  appendHistory (Need (goals `zip` modtimes))
  return modtimes

cake :: Cake () -> IO ()
cake act = do
  mst <- newMVar (CakeState{ csRules = [], csActs = [] })
  unCake act mst
  st <- takeMVar mst
  let rules = reverse (csRules st)
      acts = reverse (csActs st)
  db <- loadDatabase databaseFilePath
  printDatabase db
  mdb <- newMVar db
  logLock <- newMVar ()
  withPool 2 $ \pool -> do
    let env = ActEnv{ aeDatabase = mdb,
                      aeRules = reverse rules,
                      aePool = pool,
                      aeLogLock = logLock,
                      aeNestLevel = 0 }
    -- This is where we kick of the actual work
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
databaseFilePath = ".cakefile-db"

loadDatabase :: FilePath -> IO Database
loadDatabase fp =
  handleDoesNotExist (return (DB M.empty)) $ decodeFile fp

writeDatabase :: FilePath -> Database -> IO ()
writeDatabase = encodeFile

-- | Get file modification time (if the file exists).
getFileModTime :: CanonicalFilePath -> IO (Maybe ModTime)
getFileModTime cfp =
  handleDoesNotExist (return Nothing) $
    Just . M <$> getModificationTime (canonicalFilePath cfp)

-- | Do an action in case we get a file-does-not-exist exception.
handleDoesNotExist :: IO a  -- ^ Do this in case of error
                   -> IO a  -- ^ Do this otherwise
                   -> IO a
handleDoesNotExist = handleIf isDoesNotExistError

handleIf :: Exception.Exception e => (e -> Bool) -> IO a -> IO a -> IO a
handleIf p handler act =
  Exception.handleJust (guard . p) (\() -> handler) act