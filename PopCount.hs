{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE "popcount.h" #-}

module PopCount (popCount, slowPopCount) where

import Data.Bits
import Data.Word
import Foreign.C

foreign import ccall "popcount.h popcount" c_popcount :: CUInt -> CUInt

-- Cribbed from http://hackage.haskell.org/trac/ghc/ticket/3563
-- We should figure out what its performance characteristics are
slowPopCount :: Word -> Int
slowPopCount x = count' (bitSize x) x 0
  where
  count' 0 _ acc = acc
  count' n x acc = count' (n-1) (x `shiftR` 1) (acc + if x .&. 1 == 1 then 1 else 0)

popCount :: Word -> Int
popCount w = fromIntegral (c_popcount (fromIntegral w))

