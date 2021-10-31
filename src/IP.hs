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
import MAC

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
    netmask = intToMask prefixBits
    hostmask = intToMask (-prefixBits)

    net  = ip `masked` netmask

    subnetSize = 2^(32 - prefixBits)

  in   filter (isProper hostmask)
     $ drop 1 $ dropWhile (/=ip)
     $ genericTake subnetSize
     $ enumFrom net

isProper :: IPv4 -> IPv4 -> Bool
isProper hostmask ip = let
    hostid = ip `masked` hostmask
  in hostid /= (readIP "0.0.0.0") && hostid /= hostmask

addrMaskHost :: Addr a => Address a -> Address a
addrMaskHost (ip, prefixBits) =
    (ip `masked` (intToMask prefixBits), prefixBits)

buildEUI64 :: IPv6 -> MAC -> IPv6
buildEUI64 ip mac =
    let (a,b,_,_) = toHostAddress6 ip
        (c,d) = toModifiedEUI64 mac
        in
    fromHostAddress6 (a, b, c, d)
