module MAC where

import GHC.Generics
import Control.DeepSeq
import Numeric
import Data.Bits
import Data.Word
import Data.List
import Data.List.Split
import Data.Function

import BitUtils

newtype MAC = MAC [Word8]
    deriving (Eq, Ord, Show, Read, Generic)

instance NFData MAC

enumerateMACs :: MAC -> [MAC]
enumerateMACs (MAC bs) = let
    x = mergeWords bs :: Word64
  in
    genericTake (2^(8*6) - x) $ map (MAC . drop 2 . splitWord) $ enumIntegral x

showMAC :: MAC -> String
showMAC (MAC ws) = intercalate ":" $ map (fill . flip showHex "") ws
 where
   fill s | length s == 1 = '0':s
   fill s = s

readMAC :: String -> MAC
readMAC str = MAC $ map read $ splitOn ":" str

nullMAC = readMAC "02:00:00:00:00:00"
