module Iface (publicInterfaceResource) where

import Control.Monad
import Data.Char
import Data.Bool
import Data.Maybe
import System.Directory
import System.Process
import System.FilePath

import Types
import Resource
import Utils
import Config

publicInterfaceResource :: Interface -> Address -> [VmName] -> Resource
publicInterfaceResource (unIface -> upstream) addr vms = ManyResources $ [
    FileResource {
      rNormalize = unlines . concatMap ifupdownNormalize . lines,
      rPath = etcdir </> "intefaces.d/kipubr",
      rContent = br upstream ifn vms (net addr)
    } ] ++ map ifaceRes vms

 where
   ifn = "kipubr"
   ifpf v = ifn ++ "-" ++ v

   ifaceRes vm = IOResource {
      rCheck = bool (const Nothing) Just <$> ifexists i <*> ifstate i,
      rNeedsUpdate = (/=Just (IfState "up")),
      rUpdate = \mu -> do
          when (isNothing mu) $ pro $ tap "add" i usr
          pro $ setIfstate i (IfState "up")
    }
      where
        i = mkIface $ ifpf vm
        usr = usrpf vm

net (addr, nmask, gw) = [
  "address        " ++ addr,
  "netmask        " ++ nmask,
  "gateway        " ++ gw
 ]

br upstream ifn vms lines = iface ifn $ [
  "bridge_ports   " ++ unwords (upstream:(map ifpf vms)),
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
  "pre-up" ++ unwords (tap "add" (ifpf vm) (usrpf vm)),
  "pre-up" ++ unwords (setIfstate (ifpf vm) (IfState "up")),
  "post-down" ++ unwords (setIfstate (ifpf vm) (IfState "down")),
  "post-down"  ++ unwords (tap "del" (ifpf vm) (usrpf vm)),
  ""
 ]

usrpf u = "kib-" ++ u

data IfState = IfState String deriving (Eq, Show)
type ShCommand = [String]

isUp (IfState "up") = True
isUp _ = False

ifexists (unIface -> ifname) = doesDirectoryExist $ "/sys/class/net/" ++ ifname

tap mod (unIface -> ifname) owner =
    ["ip", "tuntap", mod, "dev", ifname, "mode", "tap", "user", owner]

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
