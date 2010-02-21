{-# OPTIONS -cpp -fglasgow-exts #-}
{-# LANGUAGE NoBangPatterns, ForeignFunctionInterface #-}
{-# INCLUDE "popcount.h" #-}

module PopCount (popCount, slowPopCount) where

import Data.Bits
import Data.Word
import Foreign.C

foreign import ccall "popcount.h popcount" c_popcount :: CUInt -> CInt

-- Cribbed from http://hackage.haskell.org/trac/ghc/ticket/3563
-- We should figure out what its performance characteristics are
slowPopCount :: Word -> Int
slowPopCount x = count' (bitSize x) x 0
  where
  count' 0 _ acc = acc
  count' n x acc = count' (n-1) (x `shiftR` 1) (acc + if x .&. 1 == 1 then 1 else 0)

{- Doesn't work
-- Depends on Word being 32-bit
popCount :: Word -> Int
popCount x
  = fromIntegral $ case x - ((shiftR x 1) .&. 0x55555555) of
    x -> case (x .&. 0x33333333) + ((shiftR x 2) .&. 0x33333333) of
      x -> shiftR ((x + (shiftR x 4) .&. 0xF0F0F0F) * 0x1010101) 24
-}

popCount :: Word -> Int
popCount w = fromIntegral (c_popcount (fromIntegral w))

