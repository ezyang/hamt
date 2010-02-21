import Data.IntMap

main = print $ m ! 100
    where m = fromList [(i,i) | i <- [1..1000000]] :: IntMap Int
