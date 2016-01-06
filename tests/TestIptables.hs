{-# LANGUAGE ViewPatterns, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeFamilies, GADTs, DataKinds, StandaloneDeriving, TupleSections #-}
module Main where

import Iptables
import Resource
import Types

import Data.List
import Data.Function
import qualified Data.Map as Map

import IP

import System.IO


main = do
  is <- parse <$> getContents
  let cfg = Config undefined "eth0" undefined undefined undefined undefined
  let vm = Vm "foo"
              defVmCfg
              defVmSysCfg
              defVmNetCfg { vPublicIf = True, vForwardedPorts4 = [ (TCP, (443, 443))
                                                                 , (UDP, (553, 53))] }
              defVmQCfg
  let is' = updateTables IPvv4 cfg (mkIface "kipubr") [vm] (Map.fromList [("foo", (undefined, readIP "10.10.10.10"))]) is
  let ppis = unparse is'
  print is'
  hPutStrLn stderr ppis

--  putStrLn "------"

--  putStr $ unparse $ disown x
