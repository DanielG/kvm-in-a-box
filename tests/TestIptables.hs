{-# LANGUAGE ViewPatterns, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeFamilies, GADTs, DataKinds, StandaloneDeriving, TupleSections #-}
module Main where

import Iptables
import Resource

import Data.List
import Data.Function


main = do
  iss <- filterKib . unIptablesSave . parse <$> getContents
  print iss

--  putStrLn "------"

--  putStr $ unparse $ disown x
