module Development.Cake
  ( need, Core.cake, want, Cake, Act,

    cat, (*>)  
  )
where

import Development.Cake.Core ( Generates(..), Act, Cake, ModTime )
import qualified Development.Cake.Core as Core

import Control.Concurrent ( threadDelay )
import Control.Monad ( guard )
import Control.Monad.IO.Class
import System.Exit
import System.FilePath
import System.FilePath.Canonical
import System.Process
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.Glob as Glob

import Debug.Trace

-- | Concatenate the contents of the source files and write the result
-- into the destination file.
cat :: [FilePath] -> FilePath -> Act ()
cat srcs dest = do
  need srcs
  liftIO $ do
    inps <- mapM (L.readFile) srcs
    L.writeFile dest (L.concat inps)

-- | Copy the source file to the destination location.
copy :: FilePath
     -> FilePath
     -> Act ()
copy from to = do
  need [from]
  liftIO $ L.writeFile to =<< L.readFile from

-- copyAll [(FilePath, FilePath)] -> Act ()
-- copyAll pairs = do
--   let (inputs, _) = unzip pairs
--   need inputs
--   mapM_ (uncurry copy) pairs
-- 
-- To perform the copies in parallel, we could generate multiple rules,
-- and just ask for their outputs.  Not sure how that would work with
-- pattern rules, though.


type Pattern = String

(*>) :: Pattern -> (FilePath -> Act ()) -> Cake ()
(*>) pattern action = do
  pwd <- liftIO (canonical ".")
  addRule $ \fp -> do
    let relfp = makeRelative (canonicalFilePath pwd) fp
    let matchp = (Glob.match compiled relfp)
    guard matchp --(trace ("matchp, fp: " ++ show (matchp, fp, pattern)) matchp)
    return ([fp], action fp)
 where compiled = Glob.compile pattern

need :: [FilePath] -> Act [ModTime]
need fps = do
  cfps <- mapM (liftIO . canonical) fps
  Core.need cfps

want :: [FilePath] -> Cake ()
want fps = Core.queueAct (need fps >> return ())

type CreatesFiles = [FilePath]
type Rule = FilePath -> Maybe (CreatesFiles, Act ())

addRule :: Rule -> Cake ()
addRule rule = Core.addRule $ \cfp -> do
  -- TODO: Save rule creating working directory somewhere?
  let fp = canonicalFilePath cfp
  case rule fp of
    Nothing -> return Nothing
    Just (creates, act) -> do
      creates_canon <- mapM canonical creates
      return (Just Generates{ genOutputs = creates_canon
                            , genAction = do
                                act
                                mapM getCleanModTime creates_canon })
 where
   getCleanModTime :: CanonicalFilePath -> Act ModTime
   getCleanModTime cfp = do
     mb_modtime <- liftIO (Core.getFileModTime cfp)
     case mb_modtime of
       Just modtime -> return modtime
       Nothing ->
         liftIO $ Core.cakeError $
           "Rule promised to create file, but didn't: " ++ show cfp

-- TODO: Is 'String' the right type for arguments?  Can there be
-- encoding issues?  Would ByteString be better.

system :: String -> [String] -> Act ()
system cmd args = do
  exitCode <- liftIO $ rawSystem cmd args
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure n ->
      fail $ "Command failed (exit code: " ++ show n ++ "): " ++
        show cmd ++ show args

system' :: String -> [String] -> Act ExitCode
system' cmd args = liftIO $ rawSystem cmd args

--ignoreErrors :: Act a -> Act ()
--ignoreErrors act = error "NYI"