module HAMT where

import Prelude hiding (lookup, (++))
import Data.Bits
import Data.Word

import Data.Vector (Vector, (++), singleton, unsafeDrop, unsafeTake, unsafeIndex, unsafeUpdate)
import qualified Data.Vector as Vector

import PopCount

type Key    = Word
type Bitmap = Word
type Shift  = Int
type Subkey = Int -- we need to use this to do shifts, so an Int it is

-- These architecture dependent constants

bitsPerSubkey :: Int
bitsPerSubkey = floor . logBase 2 . fromIntegral . bitSize $ (undefined :: Word)

subkeyMask :: Bitmap
subkeyMask = 1 `shiftL` bitsPerSubkey - 1

data HAMT a = Empty
            | BitmapIndexed {-# UNPACK #-} !Bitmap !(Vector (HAMT a))
            | Leaf {-# UNPACK #-} !Key a
    deriving (Show)

maskIndex :: Bitmap -> Bitmap -> Int
maskIndex b m = popCount (b .&. (m - 1))

mask :: Key -> Shift -> Bitmap
mask k s = shiftL 1 (fromIntegral $ shiftR k s .&. subkeyMask)

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
        BitmapIndexed b v ->
            let m = mask k s in
            if b .&. m == 0
                then Nothing
                else lookup' k (s+bitsPerSubkey) (unsafeIndex v (maskIndex b m))

insert :: Key -> a -> HAMT a -> HAMT a
insert k v t = insert' k 0 v t

insert' :: Key -> Shift -> a -> HAMT a -> HAMT a
insert' kx s x t
    = case t of
        Empty -> Leaf kx x
        Leaf ky y
            | ky == kx  -> Leaf kx x
            | otherwise ->
                insert' kx s x $ BitmapIndexed (mask ky s) (singleton t)
        BitmapIndexed b v -> {-# SCC "i-Bitmap" #-}
            let m   = mask kx s
                i   = maskIndex b m in
            if b .&. m == 0
                then let l  = Leaf kx x
                         v' = unsafeTake i v ++ singleton l ++ unsafeDrop i v
                         b' = b .|. m
                     in BitmapIndexed b' v'
                else {-# SCC "i-Bitmap-conflict" #-}
                    let  st  = unsafeIndex v i
                         st' = insert' kx (s+bitsPerSubkey) x st
                         v'  = {-# SCC "i-Bitmap-update" #-}
                               unsafeUpdate v (singleton (i, st'))
                     in BitmapIndexed b v'

-- too lazy
fromList :: [(Key, a)] -> HAMT a
fromList = foldl (flip $ uncurry insert) empty

