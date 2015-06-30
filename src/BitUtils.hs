module BitUtils where

import Data.Bits
import Data.Word

splitWord :: (Integral a, BitSize a) => a -> [Word8]
splitWord x =
    map fromIntegral $ map (shiftAndShift x) $ reverse $ take (myBitSize x `div` 8) [0,8..]
 where
   shiftAndShift x n = shiftR (shiftL 0xff n .&. x) n

mergeWords :: (Num a, Bits a) => [Word8] -> a
mergeWords bs =
    foldr (.|.) 0 $ zipWith shiftL (map fromIntegral bs) (reverse $ take (length bs) [0,8..])

enumIntegral :: Integral a => a -> [a]
enumIntegral n = n : enumIntegral (n+1)

-- gah no FiniteBits class on ghc-7.6
class Bits a => BitSize a where
    myBitSize :: a -> Int

instance BitSize Word8 where
    myBitSize = const $ 8

instance BitSize Word16 where
    myBitSize = const $ 16

instance BitSize Word32 where
    myBitSize = const $ 32

instance BitSize Word64 where
    myBitSize = const $ 64
