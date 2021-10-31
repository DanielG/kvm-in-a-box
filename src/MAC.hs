module MAC (MAC, enumerateMACs, showMAC, readMAC, nullMAC, toModifiedEUI64) where

import GHC.Generics
import Control.DeepSeq
import Control.Arrow
import Numeric
import Data.Bits
import Data.Word
import Data.List
import Data.List.Split
import Data.Function
import Safe

import BitUtils
import Read

newtype MAC = MAC [Word8]
    deriving (Eq, Ord, Generic)

instance Show MAC where
    show = showMAC

instance Read MAC where
    readsPrec i = \s -> map (first readMAC) $ (readsPrec i :: ReadS String) s

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
readMAC str = MAC $ map (unHex . readNote "readMAC") $ splitOn ":" str

nullMAC = readMAC "02:00:00:00:00:00"

toModifiedEUI64 :: MAC -> (Word32, Word32)
toModifiedEUI64 (MAC (map fromIntegral -> [a,b,c, d,e,f])) =
    let a' = a `xor` 0x80 in
    ( (a' `shift` 24) .|.
      (b  `shift` 16) .|.
      (c  `shift`  8) .|.
      0xff
    , (d  `shift` 24) .|.
      (e  `shift` 16) .|.
      (f  `shift`  8) .|.
      0xff
    )
