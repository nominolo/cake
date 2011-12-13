module Tests.Development.Cake.Core.Types where

import Development.Cake.Core.Types

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.QuickCheck hiding ( chatty, verbose )

tests =
  [ testGroup "verbosity" $
    [ testProperty "verbosityRangeHigh" $ \n ->
        (n >= 4) ==> verbosityFromInt n == chatty
    , testProperty "verbosityRangeLow" $ \n ->
        (n <= 0) ==> verbosityFromInt n == silent
    , testCase "verbositySilent" $
        verbosityFromInt 1 @?= quiet
    , testCase "verbosityNormal" $
        verbosityFromInt 2 @?= normal
    , testCase "verbosityVerbose" $
        verbosityFromInt 3 @?= verbose
    ]
  ]
