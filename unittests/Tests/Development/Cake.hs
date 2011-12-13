module Tests.Development.Cake where

import qualified Tests.Development.Cake.Options
import qualified Tests.Development.Cake.Core.Types

import Test.Framework

tests =
  [ testGroup "Tests.Development.Cake.Core.Types"
              Tests.Development.Cake.Core.Types.tests
  , testGroup "Tests.Development.Cake.Options"
              Tests.Development.Cake.Options.tests
  ]

main = defaultMain tests