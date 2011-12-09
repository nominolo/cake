-- | Provides an Oracle for looking up environment variables.
module Development.Cake.Oracles.Env where

import Development.Cake.Core

import System.Environment

mkEnvOracle :: IO (Question -> IO (Maybe Answer))
mkEnvOracle = do
  env <- getEnvironment
  return (lookupEnv env)
 where
   lookupEnv _ (Question namespace _) | namespace /= "env" = return Nothing
   lookupEnv env (Question _ varname) =
     return . Just $ maybe [""] (\ans -> [ans]) (lookup varname env)

-- | Look up the given environment variable.  If the variable is
-- undefined returns the empty string.
env :: String -> Act String
env envvar = do
  [contents] <- query (Question "env" envvar)
  return contents

