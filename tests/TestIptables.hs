{-# LANGUAGE ViewPatterns, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeFamilies, GADTs, DataKinds, StandaloneDeriving, TupleSections #-}
module Main where

import Iptables
import Resource

import Data.List
import Data.Function

import System.IO


main = do
  is <- parse <$> getContents
  let is' = updateTables is
  let ppis = unparse is'
  print is'
  hPutStrLn stderr ppis

--  putStrLn "------"

--  putStr $ unparse $ disown x
