import PopCount

import Data.Word
import Test.QuickCheck

-- Quickcheck

instance Arbitrary Word where
    arbitrary = do let mx,mn :: Integer
                       mx = fromIntegral (maxBound :: Word)
                       mn = fromIntegral (minBound :: Word)
                   c <- choose (mx, mn)
                   return (fromIntegral c)
    coarbitrary = undefined

prop_PopCount :: Word -> Bool
prop_PopCount w = (slowPopCount w == popCount w)

main = quickCheck prop_PopCount
