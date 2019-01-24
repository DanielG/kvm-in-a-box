{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XRecordWildCards"
  , "-XViewPatterns"
  , "-XLambdaCase"
  , "-XMultiWayIf"
  , "-XNamedFieldPuns"
  , "-XNoMonomorphismRestriction"
  , "-XFlexibleContexts"
  , "-XDeriveGeneric"
  , "-XScopedTypeVariables"
  , "-idist/build/autogen/"
  , "-optP-include"
  , "-optPdist/build/doctest/autogen/cabal_macros.h"
  , "src"
  ]
