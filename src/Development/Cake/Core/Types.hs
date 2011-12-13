module Development.Cake.Core.Types
  ( Verbosity, silent, quiet, normal, verbose, chatty,
    verbosityFromInt
  )
where

-- | The verbosity level.  Used for logging.
newtype Verbosity = Verbosity Int
  deriving (Eq, Ord, Show)

-- | Highest verbosity level.
chatty :: Verbosity
chatty = Verbosity 4

-- | High verbosity level.
verbose :: Verbosity
verbose = Verbosity 3

-- | Normal verbosity level.
--
-- The standard level.
normal :: Verbosity
normal = Verbosity 2

-- | Low verbosity level.
--
-- Only very important output should occur.
quiet :: Verbosity
quiet = Verbosity 1

-- | Lowest verbosity level.
--
-- No output should occur.
silent :: Verbosity
silent = Verbosity 0

-- | Construct a verbosity level from an integer.  Used, for example,
-- by command line parser.  Values outside the range @[0..4]@ are
-- automatically clipped.  E.g.,
--
-- > verbosityFromInt 100 == chatty
-- > verbosityFromInt (-50) == silent
--
verbosityFromInt :: Int -> Verbosity
verbosityFromInt n = Verbosity (max 0 (min 4 n))
