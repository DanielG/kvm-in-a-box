module Dnsmasq (allocateHosts, vmDnsDhcpResource, vmHostLeaseResource) where

import Control.Applicative
import Control.Arrow
import Data.IP
import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Function
import System.FilePath

import Resource
import Config
import Types
import Utils
import MAC
import IP

vmDnsDhcpResource :: [Interface] -> Config -> Resource
vmDnsDhcpResource bridges cfg =
  SimpleFileResource {
    rPath = etcdir </> "dnsmasq.d/kib",
    rOwner = OwnerKib,
    rNormalize = id,
    rContent =
       unlines $ [ "domain="++cDomain cfg
                 , "dhcp-hostsfile=/etc/dnsmasq.kib.hosts"
                 , "enable-ra"
                 , "dhcp-range="++(showIP $ fst $ cAddress cfg)++",static"
                 ] ++
                 ((\(unIface -> ifn) -> "dhcp-range=::,constructor:"++ifn++",slaac") `map` bridges)
 }

vmHostLeaseResource :: Address IPv4 -> [VmName] -> Resource
vmHostLeaseResource addr vmns =
  FileResource {
    rPath = kibHostsFile,
    rNormalize = unparse . sort . parse,
    rParse = addOwner . parse,
    rUnparse = unparse,
    rContentFunc = addOwner . allocate addr vmns . map snd . fromMaybe []
 }

addOwner = map (\x@(vmn,_) -> (OwnerVm vmn, x))

kibHostsFile = etcdir </> "dnsmasq.kib.hosts"

allocateHosts :: FilePath -> Address IPv4 -> [VmName] -> IO [(VmName, (MAC, IPv4))]
allocateHosts root addr vmns = do
  allocate addr vmns . parse . fromMaybe ""
    <$> readFileMaybe (rootRel root kibHostsFile)

allocate :: Address IPv4 -> [VmName] -> [(VmName, (MAC, IPv4))] -> [(VmName, (MAC, IPv4))]
allocate (ip, nm) newHosts oldHosts = let
--    oldHostsMap = Map.fromList oldHosts
--    notNeededAnymore = foldr Map.delete oldHostsMap newHosts

    maxMac = maximum $ nullMAC : map (fst . snd) oldHosts
    maxIp  = maximum $ ip : map (snd . snd) oldHosts

    macs = drop 1 $ enumerateMACs maxMac
    ips  = enumerateIPs maxIp nm

    thingsIHaveToAllocateNow = newHosts \\ map fst oldHosts
  in
    oldHosts ++ thingsIHaveToAllocateNow `zip` (macs `zip` ips)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

untup3 :: (a,b,c) -> (a,(b,c))
untup3 (a,b,c) = (a,(b,c))

tup3 :: (a,(b,c)) -> (a,b,c)
tup3 (a,(b,c)) = (a,b,c)

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
thd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x

host vmn mac ip = intercalate "," [showMAC mac, showIP ip, vmn, "infinite"]

unhost [mac, ip, vmn, ttl] = (vmn, readMAC mac, readIP ip)

unparse :: [(VmName, (MAC, IPv4))] -> String
unparse = unlines . sort . map (uncurry3 host . tup3)

parse :: String -> [(VmName, (MAC, IPv4))]
parse = map ( untup3 . unhost . splitOn ",")
      . filter (not . all isSpace)
      . lines

{-
domain=example.net
dhcp-range=::,static
dhcp-host=00:20:e0:3b:13:af,10.42.0.1,machine-hostname,infinite
-}
