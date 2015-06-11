module Types where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char
import Data.List
import Data.List.Split

type VmName = String
newtype Interface = Iface String
type GroupInterface = String
type Address = (String, String, String)
newtype MAC = MAC String
    deriving (Eq, Ord, Show, Read)

mkIface ifn =
    if all (\c -> isAlphaNum c || c == '-') ifn
       then Iface ifn
       else error $ "Invalid interface: " ++ ifn

unIface (Iface ifn) = ifn

parseMac = readMac . splitOn ":"
 where
   readMac mac@[a:b:c:d:e:f] = Right $ MAC $ intercalate ":" mac
   readMac _ = Left "invalid MAC address"

unMac (MAC m) = m

data State = State {
      sVms :: Map VmName (Vm, VmVolatileState, VmSolidState)
    } deriving (Eq, Ord, Show, Read)

defState = State Map.empty

data Vm = Vm {
      vName      :: VmName,
      vCpus      :: Int,
      vMem       :: Int,
      vArch      :: String
    } deriving (Eq, Ord, Show, Read)

data VmSolidState = VmSolidState {
-- TODO: lvm disk handling
    } deriving (Eq, Ord, Show, Read)

data VmVolatileState = VmVolatileState {
      vUp        :: Maybe Bool,
      vPublicIf  :: Maybe (Maybe MAC),
      vPrivateIf :: Maybe (Maybe MAC),
      vGroupIfs  :: Maybe (Set GroupInterface)
    } deriving (Eq, Ord, Show, Read)

defVmVolatileState = VmVolatileState (Just True) (Just Nothing) (Just Nothing) (Just Set.empty)
