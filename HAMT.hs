{-# OPTIONS -cpp -fglasgow-exts #-}
{-# LANGUAGE NoBangPatterns #-}

module HAMT where

import Prelude hiding (lookup, (++), take, drop)
import Data.Vector hiding (empty, fromList, foldl)
import Data.Bits

import PopCount

#if __GLASGOW_HASKELL__ >= 503
import GHC.Word
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import Word
import GlaExts ( Word(..), Int(..), shiftRL# )
#else
import Data.Word
#endif

type Key    = Word
type Bitmap = Word
type Shift  = Int
type Subkey = Int -- we need to use this to do shifts, so an Int it is

-- This is architecture dependent
bitsPerSubkey :: Int
bitsPerSubkey = floor . logBase 2 . fromIntegral . bitSize $ (undefined :: Word)

subkeyMask :: Bitmap
subkeyMask = 1 `shiftL` bitsPerSubkey - 1

-- GHC fun (inline by unboxing)
shiftRL :: Word -> Int -> Word
#if __GLASGOW_HASKELL__
shiftRL (W# x) (I# i) = W# (shiftRL# x i)
#else
shiftRL x i = shiftR x i
#endif

data HAMT a = Empty
            | BitmapIndexed {-# UNPACK #-} !Bitmap (Vector (HAMT a))
            | Leaf {-# UNPACK #-} !Key a
    deriving (Show)

keyIndex :: Bitmap -> Subkey -> Int
keyIndex b sk = maskIndex b (mask sk)

maskIndex :: Bitmap -> Bitmap -> Int
maskIndex b m = popCount (b .&. (m - 1))

subkey :: Key -> Shift -> Subkey
subkey w s = fromIntegral $ shiftRL w s .&. subkeyMask

mask :: Subkey -> Bitmap
mask w = shiftL 1 w

empty :: HAMT a
empty = Empty

lookup :: Key -> HAMT a -> Maybe a
lookup k t = lookup' k 0 t

lookup' :: Key -> Shift -> HAMT a -> Maybe a
lookup' k s t
    = case t of
        Empty -> Nothing
        Leaf kx x
            | k == kx   -> Just x
            | otherwise -> Nothing
        BitmapIndexed b v -> bitmapLookup k s b v

myIndex v i = v ! i -- make unsafe later

bitmapLookup :: Key -> Shift -> Bitmap -> Vector (HAMT a) -> Maybe a
bitmapLookup k s b v
    = let sk = subkey k s in
        if testBit b sk
            then lookup' k (s+bitsPerSubkey) (myIndex v (keyIndex b sk))
            else Nothing

insert :: Key -> a -> HAMT a -> HAMT a
insert k v t = insert' k 0 v t

insert' :: Key -> Shift -> a -> HAMT a -> HAMT a
insert' kx s x t
    = case t of
        Empty -> {-# SCC "i-Empty" #-} Leaf kx x
        Leaf ky y
            | ky == kx  -> {-# SCC "i-Leaf-replace" #-} Leaf kx x
            | otherwise -> {-# SCC "i-Leaf-conflict" #-}
                let t' = BitmapIndexed m (singleton t)
                    m  = mask (subkey ky s)
                in insert' kx s x t'
        BitmapIndexed b v ->
            let skx = subkey kx s
                m   = mask skx
                i   = maskIndex b m in
            if testBit b skx
                then {-# SCC "i-Bitmap-conflict" #-}
                    let  i   = keyIndex b skx
                         st  = myIndex v i
                         st' = insert' kx (s+bitsPerSubkey) x st
                         v'  = update v (singleton (i, st'))
                     in BitmapIndexed b v'
                else {-# SCC "i-Bitmap-insert" #-}
                     let l  = Leaf kx x
                         v' = take i v ++ singleton l ++ drop i v
                         b' = b .|. m
                     in BitmapIndexed b' v'

-- too lazy
fromList :: [(Key, a)] -> HAMT a
fromList = foldl (flip $ uncurry insert) empty

