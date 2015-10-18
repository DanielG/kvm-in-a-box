module Iface (interfaceResource) where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Bool
import Data.Maybe
import Data.IP
import System.Directory
import System.Process
import System.FilePath
import System.Exit

import Types
import Resource
import Utils
import Files
import IP

interfaceResource :: Interface -> Maybe (Address IPv4) -> Address IPv6 -> [VmName] -> Bool -> Resource
interfaceResource (unIface -> ifn) maddr addr6 vms amRoot = ManyResources $ [
    SimpleFileResource {
      rPath = etcdir </> "network/interfaces.d/"++ifn,
      rNormalize = unlines . concatMap ifupdownNormalize . lines,
      rContent = br ifn addr6 vms $ fromMaybe [] $ net <$> maddr,
      rOwner = OwnerKib
    } ] ++ if amRoot then map ifaceRes vms else []

 where
   ifpf v = ifn ++ "-" ++ v

   ifaceRes vm = IOResource {
      rUpdateMsg = \case
        Nothing -> "interface for VM '"++vm++"' doesn't exist, configuring"
        Just (IfState "down") -> "interface for VM '"++vm++"' down, upping",

      rCheck = do
        e <- ifexists i
        if e
          then Just <$> ifstate i
          else return Nothing,

      rNeedsUpdate = (/=Just (IfState "up")),
      rUpdate = \mu -> do
          when (isNothing mu) $ do
            pro $ tapAdd i usr
            return ()
          pro $ setIfstate i (IfState "up")
          return ()
    }
      where
        i = mkIface $ ifpf vm
        usr = usrpf vm

net (ip, prefix) = [
  "address        " ++ showIP ip,
  "netmask        " ++ show prefix
 ]

br ifn addr6 vms lines = iface ifn $
 br_ports ++ [
  "bridge_stp     off",
  "bridge_maxwait 0",
  "bridge_fd      0"
 ] ++ lines ++ concatMap (downstreamIface (mkIface ifn) (mkIface . ifpf) addr6) vms
 where
   ifpf v = ifn ++ "-" ++ v
   br_ports | not $ null vms = [("bridge_ports   " ++ unwords (map ifpf vms))]
            | otherwise = []

iface name lines = unlines $ [
  "auto "++name,
  "iface "++name++" inet static"
  -- static because downstream interfaces could hijack address by running a
  -- dhcp server themselves
 ] ++ map ("  " ++) lines

downstreamIface ifn ifpf addr6 vm = [
  "pre-up " ++ unwords (tapAdd (ifpf vm) (usrpf vm)),
  "pre-up " ++ unwords (setIfstate (ifpf vm) (IfState "up")),
  "up " ++ unwords (modIpv6 "add" ifn addr6),
-- TODO: set static arp entry and filter everything else using arptables
--  "pre-up" ++ unwords (arp (ifpf vm) mac),
  "down " ++ unwords (modIpv6 "del" ifn addr6),
  "post-down " ++ unwords (setIfstate (ifpf vm) (IfState "down")),
  "post-down " ++ unwords (tapDel (ifpf vm))
 ]

usrpf u = "kib-" ++ u

data IfState = IfState String deriving (Eq, Show)
type ShCommand = [String]

isUp (IfState "up") = True
isUp _ = False

ifexists (unIface -> ifname) = doesDirectoryExist $ "/sys/class/net/" ++ ifname

tapAdd (unIface -> ifname) owner =
    ["ip", "tuntap", "add", "dev", ifname, "mode", "tap", "user", owner]

tapDel (unIface -> ifname) =
    ["ip", "tuntap", "del", "dev", ifname, "mode", "tap"]

arp (unIface -> ifname) ip mac =
    ["arp", "-i", ifname, "-s", ip, mac]

modIpv6 :: String -> Interface -> (IPv6, Prefix) -> [String]
modIpv6 action (unIface -> ifname) iprange6 =
    ["ip", "-6", "addr", action, showIPRange iprange6, "dev", ifname]

ifstate :: Interface -> IO IfState
ifstate (unIface -> ifname) = do
  s <- readFile $ "/sys/class/net/"++ifname++"/operstate"
  return $ case s of
    "up" -> IfState "up"
    _    -> IfState "down"

setIfstate :: Interface -> IfState -> ShCommand
setIfstate (unIface -> ifname) (IfState ifs) = do
  ["ip", "link", "set", ifname, ifs]

ifupdownNormalize :: String -> [String]
ifupdownNormalize ('#':l) = []
ifupdownNormalize (c:l) | isSpace c = [' ' : wuws (dropWhile isSpace l)]
ifupdownNormalize l = [wuws l]

wuws = unwords . words
