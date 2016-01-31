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
import JSON

data Config = Config {
      cDomain    :: String,
      cInterface :: String,
      cAddress   :: Address IPv4,
      cAddress6  :: Address IPv6,
      cPrivate6  :: Address IPv6,
      cGroup6    :: Address IPv6
    } deriving Show

type VmName = String
newtype Interface = Iface String deriving (Eq, Show)
type GroupInterface = String

data IPv = IPvv4 | IPvv6 deriving (Eq, Ord, Read, Show, Generic)
data Proto = UDP | TCP deriving (Eq, Ord, Read, Show, Generic)

instance NFData IPv
instance NFData Proto

mkProto :: String -> Proto
mkProto str | "udp" <- map toLower str = UDP
mkProto str | "tcp" <- map toLower str = TCP

unProto :: Proto -> String
unProto UDP = "udp"
unProto TCP = "tcp"

mkIface ifn =
    if all (\c -> isAlphaNum c || c == '-') ifn
       then Iface ifn
       else error $ "Invalid interface: " ++ ifn

unIface (Iface ifn) = ifn

flagTH [d|
 -- | virtual machine type configuration
 data VmCfg = VmCfg {
       vArch      :: String
     } deriving (Eq, Ord, Show, Read, Generic)
 |]

defVmCfg = VmCfg "x86_64"


flagTH [d|
 data VmSysCfg = VmSysCfg {
 -- | VM system configuration, i.e. stuff we setup on the host system
       vVg :: String,
       vAddDisks :: [String],
       -- TODO: lvm disk handling

       vAuthorizedKeys :: [String]
     } deriving (Eq, Ord, Show, Read, Generic)
 |]

defVmSysCfg = VmSysCfg "vg0" [] []


flagTH [d|
 -- | VM network configuration
 data VmNetCfg = VmNetCfg {
       vUserIf    :: Bool,
       vPublicIf  :: Bool,
       vPrivateIf :: Bool,
       vGroupIfs  :: Set GroupInterface,
       vForwardedPorts4 :: [(Proto, (Word16, Word16))],
       vOpenPorts6 :: [(Proto, Word16)] -- TODO
     } deriving (Eq, Ord, Show, Read, Generic)
 |]

defVmNetCfg = VmNetCfg False False False Set.empty [] []

flagTH [d|
 -- | VM quota configuration
 data VmQCfg = VmQCfg {
       vCpus      :: Int,
       vMem       :: Int
     } deriving (Eq, Ord, Show, Read, Generic)
 |]

defVmQCfg = VmQCfg 1 512

data Vm = Vm {
       vName      :: VmName,
       vCfg       :: VmCfg,
       vSysCfg    :: VmSysCfg,
       vNetCfg    :: VmNetCfg,
       vQCfg      :: VmQCfg
     } deriving (Eq, Ord, Show, Read, Generic)

data VmFlags = VmFlags {
       vCfgFlag    :: VmCfgFlags,
       vSysCfgFlag :: VmSysCfgFlags,
       vNetCfgFlag :: VmNetCfgFlags,
       vQCfgFlag   :: VmQCfgFlags
     } deriving (Eq, Ord, Show, Read, Generic)

combineVmFlags (VmFlags a b c d) (VmFlags a' b' c' d') =
    VmFlags
      (combineVmCfgFlags a a')
      (combineVmSysCfgFlags b b')
      (combineVmNetCfgFlags c c')
      (combineVmQCfgFlags d d')

unVmFlags name (VmFlags a b c d) =
    Vm
      name
      (unVmCfgFlags a)
      (unVmSysCfgFlags b)
      (unVmNetCfgFlags c)
      (unVmQCfgFlags d)

mkVmFlags Vm {..} =
    VmFlags
      (mkVmCfgFlags vCfg)
      (mkVmSysCfgFlags vSysCfg)
      (mkVmNetCfgFlags vNetCfg)
      (mkVmQCfgFlags vQCfg)

defVm name = Vm name defVmCfg defVmSysCfg defVmNetCfg defVmQCfg

defVmFlags = mkVmFlags $ defVm (error "defVmFlags: name undefined")

data State = State {
      sVms :: Map VmName Vm,
      sNet :: Map VmName (MAC, IPv4)
    } deriving (Eq, Ord, Show, Read, Generic)

defState = State Map.empty Map.empty

instance NFData Vm
instance NFData VmCfg
instance NFData VmSysCfg
instance NFData VmNetCfg
instance NFData VmQCfg

deriveJSON jsonOpts ''State
deriveJSON jsonOpts ''Vm
deriveJSON jsonOpts ''VmCfg
deriveJSON jsonOpts ''VmSysCfg
deriveJSON jsonOpts ''VmNetCfg
deriveJSON jsonOpts ''VmQCfg
deriveJSON jsonOpts ''Proto

instance FromJSON MAC where
    parseJSON (String v) = maybe (fail "FromJSON MAC") return $ Just $ readMAC $ unpack v
    parseJSON _          = mzero

instance ToJSON MAC where
    toJSON mac = String $ pack $ show mac

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
