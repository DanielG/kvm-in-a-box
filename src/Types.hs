{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.IP
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Data.Char
import Data.List
import Data.List.Split
import Data.Aeson.TH
import Data.Aeson
import Data.Text (unpack, pack)
import Text.Read hiding (String)

import Control.DeepSeq
import Control.Monad

import GHC.Generics

import FlagTH

import MAC
import IP

data Config = Config {
      cDomain    :: String,
      cInterface :: String,
      cAddress   :: Address IPv4,
      cAddress6  :: Address IPv6,
      cPrivate6  :: Address IPv6,
      cGroup6    :: Address IPv6
    } deriving Show

type VmName = String
newtype Interface = Iface String
type GroupInterface = String

mkIface ifn =
    if all (\c -> isAlphaNum c || c == '-') ifn
       then Iface ifn
       else error $ "Invalid interface: " ++ ifn

unIface (Iface ifn) = ifn

flagTH [d|
 data VmSS = VmSS {
       vVg :: String
 -- TODO: lvm disk handling
     } deriving (Eq, Ord, Show, Read, Generic)
 |]

defVmSS = VmSS "kib"

flagTH [d|
 data VmVS = VmVS {
       vCpus      :: Int,
       vMem       :: Int,
       vArch      :: String,
       vUserIf    :: Bool,
       vPublicIf  :: Bool,
       vPrivateIf :: Bool,
       vGroupIfs  :: Set GroupInterface
     } deriving (Eq, Ord, Show, Read, Generic)
 |]

defVmVS = VmVS 1 512 "x86_64" False False False Set.empty

data Vm = Vm {
      vName      :: VmName,
      vSS        :: VmSS,
      vVS        :: VmVS
    } deriving (Eq, Ord, Show, Read, Generic)

data VmFlags = VmFlags {
      vSsFlag        :: VmSSFlags,
      vVsFlag        :: VmVSFlags
    } deriving (Eq, Ord, Show, Read, Generic)

combineVmFlags (VmFlags a b) (VmFlags a' b') =
    VmFlags (combineVmSSFlags a a') (combineVmVSFlags b b')

unVmFlags name (VmFlags a b) =
    Vm name (unVmSSFlags a) (unVmVSFlags b)

mkVmFlags Vm {..} = VmFlags (mkVmSSFlags vSS) (mkVmVSFlags vVS)

defVmFlags = mkVmFlags $ Vm (error "defVmFlags: undefined") defVmSS defVmVS

data State = State {
      sVms :: Map VmName Vm,
      sNet :: Map VmName (MAC, IPv4)
    } deriving (Eq, Ord, Show, Read, Generic)

defState = State Map.empty Map.empty

--instance NFData State
instance NFData Vm
instance NFData VmSS
instance NFData VmVS

deriveJSON defaultOptions ''State
deriveJSON defaultOptions ''IP
deriveJSON defaultOptions ''MAC
deriveJSON defaultOptions ''Vm
deriveJSON defaultOptions ''VmSS
deriveJSON defaultOptions ''VmVS


instance FromJSON IPv4 where
    parseJSON (String v) = maybe (fail "FromJSON IPv4") return $ readMaybe $ unpack v
    parseJSON _          = mzero

instance ToJSON IPv4 where
    toJSON ip = String $ pack $ show ip

instance FromJSON IPv6 where
    parseJSON (String v) = maybe (fail "FromJSON IPv4") return $ readMaybe $ unpack v
    parseJSON _          = mzero

instance ToJSON IPv6 where
    toJSON ip = String $ pack $ show ip
