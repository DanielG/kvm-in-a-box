module Iface (interfaceResource) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Writer
import Control.Exception
import Data.Char
import Data.Bool
import Data.Maybe
import Data.IP
import System.Directory
import System.Process
import System.FilePath
import System.Exit
import System.IO.Error

import Types
import Resource
import Utils
import Files
import IP

interfaceResource :: Interface -> Maybe (Address IPv4) -> Address IPv6 -> [VmName] -> Bool -> Resource
interfaceResource br@(unIface -> brn) maddr addr6 vms amRoot = ManyResources $ [
    SimpleFileResource {
      rPath = etcdir </> "network/interfaces.d/"++brn,
      rPerms = ((Nothing, Nothing), Just "644"),
      rNormalize = unlines . concatMap ifupdownNormalize . lines,
      rContent = brdef brn addr6 vms $ fromMaybe [] $ net <$> maddr,
      rOwner = OwnerKib
    } ] ++ if amRoot then map ifaceRes vms else []

 where
   ifpf v = brn ++ "-" ++ v

   ifaceRes vm = IOResource {
      rUpdateMsg = \(not_if_exists, if_down, not_bridge_iface) ->
        unlines $ execWriter $ do
          when not_if_exists $
            tell ["interface for VM '"++vm++"' doesn't exist, configuring"]
          when if_down $
            tell ["interface for VM '"++vm++"' down, upping"]
          when not_bridge_iface $
            tell ["interface for VM '"++vm++"' not connected to bridge, connecting"],

      rUpdate = \(not_if_exists, if_down, not_bridge_iface) -> do
        when not_if_exists $
          pro $ tapAdd i usr
        when if_down $
          pro $ setIfstate i (IfState "up")
        when not_bridge_iface $
          pro $ brAddIf br i,

      rCheck = do
        not_if_exists <- not <$> ifexists i
        if_down <- (== Right (IfState "down"))
                     <$> tryJust (guard . isDoesNotExistError) (ifstate i)
        not_bridge_iface <- (i `elem`) <$> brIfaces br
        return (not_if_exists, if_down, not_bridge_iface)
      }
    where
      i = mkIface $ ifpf vm
      usr = usrpf vm

net (ip, prefix) = [
  "address        " ++ showIP ip,
  "netmask        " ++ show prefix
 ]

brdef brn addr6 vms lines = iface brn $
 br_ports ++ [
  "bridge_stp     off",
  "bridge_maxwait 0",
  "bridge_fd      0"
 ] ++ lines ++ concatMap (downstreamIface (mkIface brn) (mkIface . ifpf) addr6) vms
 where
   ifpf v = brn ++ "-" ++ v
   br_ports | not $ null vms = [("bridge_ports   " ++ unwords (map ifpf vms))]
            | otherwise = []

iface name lines = unlines $ [
  "auto "++name,
  "iface "++name++" inet static"
  -- static because downstream interfaces could hijack address by running a
  -- dhcp server themselves
 ] ++ map ("  " ++) lines

downstreamIface brn ifpf addr6 vm = [
  "pre-up " ++ unwords (tapAdd (ifpf vm) (usrpf vm)),
  "pre-up " ++ unwords (setIfstate (ifpf vm) (IfState "up")),
  "up " ++ unwords (modIpv6 "add" brn addr6),
-- TODO: set static arp entry and filter everything else using arptables
--  "pre-up" ++ unwords (arp (ifpf vm) mac),
  "down " ++ unwords (modIpv6 "del" brn addr6),
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

brIfaces :: Interface -> IO [Interface]
brIfaces (unIface -> br) =
  map mkIface . filter (not . (`elem` [".", ".."]))
    <$> getDirectoryContents ("/sys/class/net/" </> br </> "brif")

brAddIf (unIface -> br) (unIface -> brn) =
  ["brctl", "addif", br, brn]

ifupdownNormalize :: String -> [String]
ifupdownNormalize ('#':l) = []
ifupdownNormalize (c:l) | isSpace c = [' ' : wuws (dropWhile isSpace l)]
ifupdownNormalize l = [wuws l]

wuws = unwords . words
