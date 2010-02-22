import System
import Data.IntMap

main = do
    args <- getArgs
    let m = {-# SCC "construct-intmap" #-} fromList [(i,i) | i <- [1..(read $ head args)]] :: IntMap Int
    print $ {-# SCC "lookup-intmap" #-} m ! 100
