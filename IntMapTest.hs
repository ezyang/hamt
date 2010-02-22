import System
import System.Random.Mersenne
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Bits

main = do
    args <- getArgs
    let i = read $ head args :: Int
    gen <- getStdGen
    vals <- randoms gen
    let m = IntMap.fromList [(x,x) | x <- take i vals] :: IntMap Int
        rvals = take i (drop (shiftR i 1) vals)
    print (sum [fromIntegral . maybe 0 id $ IntMap.lookup x m | x <- rvals] :: Integer)
