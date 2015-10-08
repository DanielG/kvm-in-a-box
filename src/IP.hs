{-# LANGUAGE ScopedTypeVariables #-}
module IP where

import Safe
import Numeric
import Data.IP
import Data.Word
import Data.Bits
import Data.List
import Data.List.Split
import GHC.Generics
import Control.DeepSeq
import Control.Arrow

import BitUtils

-- | CIDR prefix
type Prefix = Int
type Address a  = (a, Prefix)

showIP :: IPv4 -> String
showIP6 :: IPv6 -> String
readIP :: String -> IPv4
readIP6 :: String -> IPv6

showIP = show
showIP6 = show
readIP = readNote "IP"
readIP6 = readNote "IP6"

readIPRange :: forall ip. Read ip => String -> (ip, Prefix)
readIPRange str = let
    [(ip :: ip, '/':rest)] = reads str
 in
   (ip, readNote "readIPRange" rest)

showIPRange :: Show ip => (ip, Prefix) -> String
showIPRange (ip, pfx) = show ip ++ "/" ++ show pfx

enumerateIPs :: IPv4 -> Int -> [IPv4]
enumerateIPs _ prefixBits
    | prefixBits > 32 || prefixBits < 0 =
        error "enumerateIPs: Invalid netmask prefix."
enumerateIPs ip prefixBits = let
    x = toHostAddress ip

    netmask =
        ((1 `shiftL` prefixBits) - 1) `shiftL` (32 - fromIntegral prefixBits)
    hostmask = complement netmask

    net  = x .&. netmask
    host = x .&. hostmask

    subnetSize = 2^(32 - prefixBits)

    allIps = enumIntegral x
    netIps = filter (isProper hostmask)
           $ genericTake (subnetSize - host) allIps
  in
    map fromHostAddress netIps

isProper hostmask ip = let
    hostid = ip .&. hostmask
  in hostid /= 0 && hostid /= hostmask
