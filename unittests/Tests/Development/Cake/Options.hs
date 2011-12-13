module Tests.Development.Cake.Options where

import Development.Cake.Options
import Development.Cake.Core.Types

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests =
  [ testGroup "parseOptions" $
    [ testCase "noflags" $
        let (opts', [], []) = parseOptions [] defaultOptions in
        opts' @?= defaultOptions
    , testCase "-v" $
        let (opts', [], []) = parseOptions ["-v"] defaultOptions in
        optionVerbosity opts' @?= verbose
    , testCase "-v0" $
        let (opts', [], []) = parseOptions ["-v0"] defaultOptions in
        optionVerbosity opts' @?= silent
    , testCase "-v42" $
        let (opts', [], []) = parseOptions ["-v42"] defaultOptions in
        optionVerbosity opts' @?= chatty
    , testCase "-j" $
        let (opts', [], [_warn]) = parseOptions ["-j"] defaultOptions in
        optionThreads opts' @?= optionThreads defaultOptions
    , testCase "-j0" $
        let (opts', [], [_warn]) = parseOptions ["-j0"] defaultOptions in
        optionThreads opts' @?= 1
    , testCase "-j5" $
        let (opts', [], []) = parseOptions ["-j5"] defaultOptions in
        optionThreads opts' @?= 5
    , testCase "unknownFlags" $ do
        let (opts', rest, warns) = parseOptions ["-j5", "-v", "-xy", "--blah"]
                                             defaultOptions
        optionThreads opts' @?= 5
        optionVerbosity opts' @?= verbose
        rest @?= ["-xy", "--blah"]
        warns @?= []
    ]
  ]