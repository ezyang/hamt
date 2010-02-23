import System
import System.Random.Mersenne
import Data.Word
import Data.Maybe
import Data.Bits
import Data.HashTable
import System.TimeIt

import HAMT (HAMT)
import qualified HAMT

main = do
    args <- getArgs
    let i = read $ head args :: Int
    timeIt $ do
        gen <- getStdGen
        vals <- randoms gen
        let m = HAMT.fromList [(fromIntegral $ hashInt x,x) | x <- take i vals] :: HAMT Int
            rvals = take i (drop (shiftR i 1) vals)
        print (sum [fromIntegral . maybe 0 id $ HAMT.lookup (fromIntegral $ hashInt x) m | x <- rvals] :: Integer)

