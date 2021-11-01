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
import System.Process hiding (system, rawSystem)
import System.FilePath
import System.Exit
import System.IO.Error

import Types
import Resource
import Utils
import Files
import IP
import MAC

interfaceResource :: Interface -> Maybe (Address IPv4) -> Address IPv6 -> [(VmName, (MAC, IPv4))] -> Bool -> ManyResources
interfaceResource br@(unIface -> brn) maddr addr6 vms amRoot = ManyResources $ [
    SomeResource $ SimpleFileResource {
      sfrPath = etcdir </> "network/interfaces.d/"++brn,
      sfrPerms = ((Nothing, Nothing), Just "644"),
      sfrNormalize = unlines . concatMap ifupdownNormalize . lines,
      sfrContent = brdef br addr6 vms $ fromMaybe [] $ net <$> maddr,
      sfrOwner = OwnerKib
    } ] ++ if amRoot then map (ifaceRes . fst) vms else []

 where
   ifpf v = brn ++ "-" ++ v

   ifaceRes vm = SomeResource $ IOResource {
      rUpdateMsg = \(not_if_exists, if_down, bridge_iface) ->
        unlines $ execWriter $ do
          when not_if_exists $
            tell ["interface for VM '"++vm++"' doesn't exist, configuring"]
          when if_down $
            tell ["interface for VM '"++vm++"' down, upping"]
          when (not bridge_iface) $
            tell ["interface for VM '"++vm++"' not connected to bridge, connecting"],

      rUpdate = \(not_if_exists, if_down, bridge_iface) -> do
        when not_if_exists $
          pro $ tapAdd i usr
        when if_down $
          pro $ setIfstate i (IfState "up")
        when (not bridge_iface) $
          pro $ brAddIf br i,

      rCheck = do
        not_if_exists <- not <$> ifexists i
        if_down <- (== Right (IfState "down"))
                     <$> tryJust (guard . isDoesNotExistError) (ifstate i)
        bridge_iface <- (i `elem`) <$> brIfaces br
        return (not_if_exists, if_down, bridge_iface)
      }
    where
      i = mkIface $ ifpf vm
      usr = kibVmUser vm

net (ip, prefix) = [
  "address        " ++ showIP ip,
  "netmask        " ++ show prefix
 ]

brdef br@(unIface -> brn) addr6 vms lines = iface brn $ [
  "bridge_ports   none",
  "bridge_stp     off",
  "bridge_maxwait 0",
  "bridge_fd      0",

  "up " ++ unwords (modIpv6 "add" br addr6),
  "down " ++ unwords (modIpv6 "del" br addr6),

  "up " ++ unwords ["sysctl", "-w", "net.ipv4.conf.$IFACE.proxy_arp_pvlan=1" ],
  "up " ++ unwords ["sysctl", "-w", "net.ipv4.conf.$IFACE.send_redirects=0" ],
  "up " ++ unwords ["sysctl", "-w", "net.ipv4.conf.$IFACE.accept_redirects=0" ],
  "up " ++ unwords ["sysctl", "-w", "net.ipv6.conf.$IFACE.accept_redirects=0" ]
 ] ++ lines ++ concatMap (downstreamIface brn addr6) vms

iface name lines = unlines $ [
  "auto "++name,
  "iface "++name++" inet static"
  -- static because downstream interfaces could hijack address by running a
  -- dhcp server themselves
 ] ++ map ("  " ++) lines

downstreamIface brn (ipv6, pfx) (vmn, (mac, showIP -> ipv4)) = let
    ifn = brn ++ "-" ++ vmn
    ifa = mkIface ifn
    ipv6_ll   = showIP6 $ buildEUI64 (read "fe80::") mac
    ipv6_auto = showIP6 $ buildEUI64 ipv6 mac
  in [
  "up " ++ unwords (tapAdd ifa (kibVmUser vmn)),
  "down " ++ unwords (tapDel ifa),

  "up    ip link set dev "++ifn++" master $IFACE",
  "down  ip link set dev "++ifn++" nomaster",

  "up    bridge link set dev "++ifn++" isolated on learning off flood off mcast_flood off",
  "up    bridge fdb replace "++showMAC mac++" dev "++ifn++" master static",
  "up    ip neigh replace "++ipv4++" lladdr "++showMAC mac++" dev $IFACE nud permanent",
  "up    ip neigh replace "++ipv6_ll++" lladdr "++showMAC mac++" dev $IFACE nud permanent",
  "up    ip neigh replace "++ipv6_auto++" lladdr "++showMAC mac++" dev $IFACE nud permanent",

  "up " ++ unwords (setIfstate ifa (IfState "up")),
  "down " ++ unwords (setIfstate ifa (IfState "down"))
 ]
 where
    ifpf v = brn ++ "-" ++ v


kibVmUser u = "kib-" ++ u

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
  return $ case takeWhile isAlphaNum s of
    "up" -> IfState "up"
    _    -> IfState "down"

setIfstate :: Interface -> IfState -> ShCommand
setIfstate (unIface -> ifname) (IfState ifs) = do
  ["ip", "link", "set", ifname, ifs]

brIfaces :: Interface -> IO [Interface]
brIfaces (unIface -> br) =
  map mkIface . filter (not . (`elem` [".", ".."]))
    <$> getDirectoryContents ("/sys/class/net/" </> br </> "brif")

brAddIf (unIface -> brn) (unIface -> port) =
  ["ip", "link", "set", "dev", port, "master", brn]

ifupdownNormalize :: String -> [String]
ifupdownNormalize ('#':l) = []
ifupdownNormalize (c:l) | isSpace c = [' ' : wuws (dropWhile isSpace l)]
ifupdownNormalize l = [wuws l]

wuws = unwords . words
