import System
import Data.IntMap

main = do
    args <- getArgs
    let m = fromList [(i,i) | i <- [1..(read $ head args)]] :: IntMap Int
    print $ m ! 100
