import System
import System.Random.Mersenne
import Data.Word
import Data.Maybe
import Data.Bits

import HAMT (HAMT)
import qualified HAMT

main = do
    args <- getArgs
    let i = read $ head args :: Int
    gen <- getStdGen
    vals <- randoms gen
    let m = HAMT.fromList [(x,x) | x <- take i vals] :: HAMT Word
        rvals = take i (drop (shiftR i 1) vals)
    print (sum [fromIntegral . maybe 0 id $ HAMT.lookup x m | x <- rvals] :: Integer)

