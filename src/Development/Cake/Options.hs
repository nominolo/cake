{-# LANGUAGE BangPatterns #-}
module Development.Cake.Options
  ( Options(..), defaultOptions, parseOptions )
where

import Development.Cake.Core.Types

import Control.Applicative
import Data.Maybe ( fromMaybe, listToMaybe )
import GHC.Conc ( numCapabilities )

-- | Command line options for Cake.
data Options = Options
  { optionVerbosity :: Verbosity
    -- ^ Verbosity level of logging.
  , optionThreads :: Int
    -- ^ Maximum allowed number of simultaneous build tasks.
  } deriving (Eq, Ord, Show)

-- | Default options to use.
defaultOptions :: Options
defaultOptions = Options
  { optionVerbosity = silent
  , optionThreads = numCapabilities
  }

type Message = String

-- | Parse options from list of command line flags.
--
-- Returns the parsed options, the list of unprocessed flags, and a
-- list of warning messages for flags that failed to parse.  Currently
-- supported command line flags are:
--
-- > -v[0-4] .. set verbosity level (no argument = -v3)
-- > -j<N>   .. allow up to <N> concurrent workers, <N> >= 1
--
parseOptions :: [String] -> Options -> (Options, [String], [Message])
parseOptions args opts0 = filterArgs opts0 args [] []
 where
   filterArgs opts [] unparsed warns =
     (opts, reverse unparsed, reverse warns)

   filterArgs opts (arg:args) unparsed warns =
     case arg of
       '-':'v':n ->
         let !opts' =
               opts{ optionVerbosity =
                       fromMaybe verbose (verbosityFromInt <$> parseInt n) }
         in filterArgs opts' args unparsed warns
       '-':'j':n ->
         let (opts', warns') =
               case parseInt n of
                 Nothing ->
                   let warn = "Could not parse argument to -j: " ++ n in
                   (opts, warn:warns)
                 Just nworkers
                   | nworkers < 1
                   -> (opts{ optionThreads = 1 },
                       "Argument to -j too small, using 1" : warns)
                   | otherwise
                   -> (opts{ optionThreads = nworkers }, warns)
         in filterArgs opts' args unparsed warns'
       _ ->
         filterArgs opts args (arg:unparsed) warns

   addWarningIf True warn warns = warn:warns
   addWarningIf _    _    warns = warns

parseInt :: String -> Maybe Int
parseInt str = fst <$> listToMaybe (filter (null . snd) (reads str))
