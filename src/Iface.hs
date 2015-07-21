module Iface (interfaceResource) where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Bool
import Data.Maybe
import System.Directory
import System.Process
import System.FilePath
import System.Exit

import Types
import Resource
import Utils
import Config
import IP

interfaceResource :: Interface -> Address -> [VmName] -> Resource
interfaceResource (unIface -> ifn) addr vms = ManyResources $ [
    SimpleFileResource {
      rPath = etcdir </> "network/interfaces.d/"++ifn,
      rNormalize = unlines . concatMap ifupdownNormalize . lines,
      rContent = br ifn vms (net addr),
      rOwner = OwnerKib
    } ] ++ map ifaceRes vms

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
            ExitSuccess <- pro $ tapAdd i usr
            return ()
          ExitSuccess <- pro $ setIfstate i (IfState "up")
          return ()
    }
      where
        i = mkIface $ ifpf vm
        usr = usrpf vm

net (showIP -> addr, show -> nmask, showIP -> gw) = [
  "address        " ++ addr,
  "netmask        " ++ nmask,
  "gateway        " ++ gw
 ]

br ifn vms lines = iface ifn $ [
  "bridge_ports   " ++ unwords (map ifpf vms),
  "bridge_stp     off",
  "bridge_maxwait 0",
  "bridge_fd      0"
 ] ++ lines ++ concatMap (downstreamIface (mkIface . ifpf)) vms
 where ifpf v = ifn ++ "-" ++ v

iface name lines = unlines $ [
  "auto "++name,
  "iface "++name++" inet static"
  -- static because downstream interfaces could hijack address by running a
  -- dhcp server themselves
 ] ++ map ("  " ++) lines

downstreamIface ifpf vm = [
  "",
  "pre-up " ++ unwords (tapAdd (ifpf vm) (usrpf vm)),
  "pre-up " ++ unwords (setIfstate (ifpf vm) (IfState "up")),
-- TODO: set static arp entry and filter everything else using arptables
--  "pre-up" ++ unwords (arp (ifpf vm) mac),
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
    ["ip", "tuntap", "del", "dev"]

arp (unIface -> ifname) ip mac =
    ["arp", "-i", ifname, "-s", ip, mac]

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
