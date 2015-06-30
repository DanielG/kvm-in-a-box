module IP where

import Numeric
import Data.Word
import Data.Bits
import Data.List
import Data.List.Split
import GHC.Generics
import Control.DeepSeq
import Control.Arrow

import BitUtils

newtype IP = IP [Word8]
    deriving (Eq, Ord, Show, Read, Generic)

instance NFData IP

showIP :: IP -> String
showIP (IP cs) = intercalate "." $ map show cs

readIP :: String -> IP
readIP str = let
    ip@[_,_,_,_] = map read $ splitOn "." str
  in
    IP ip

readNetmask :: String -> Int
readNetmask s =
    case read s of
      prefixBits | prefixBits > 32 || prefixBits < 0 ->
                error "enumerateIPs: Invalid netmask prefix."
      prefixBits -> prefixBits

enumerateIPs :: IP -> Int -> [IP]
enumerateIPs _ prefixBits
    | prefixBits > 32 || prefixBits < 0 =
        error "enumerateIPs: Invalid netmask prefix."
enumerateIPs (IP bs) prefixBits = let
    x = mergeWords bs :: Word32

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
    map (IP . splitWord) netIps

    -- ( (fill32 $ bin (net :: Word32), "net")
    -- , (fill32 $ bin host, "host")
    -- , (fill32 $ bin (netmask :: Word32), "netmask")
    -- , (fill32 $ bin (hostmask :: Word32), "hostmask")
    -- , subnetSize
    -- , (subnetSize - host)
    -- , map (second $ IP . splitWord) netIps
    -- )

-- fill32 str = replicate (32 - length str) '0' ++ str

-- hex = flip showHex ""
-- bin = flip (showIntAtBase 2 (\case 0 -> '0'; 1 -> '1')) ""

isProper hostmask ip = let
    hostid = ip .&. hostmask
  in hostid /= 0 && hostid /= hostmask
