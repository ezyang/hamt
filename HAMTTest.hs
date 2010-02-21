import System
import Data.Word

import HAMT (HAMT)
import qualified HAMT

main = do
    args <- getArgs
    let m = HAMT.fromList [(i,i) | i <- [1..(read $ head args)]] :: HAMT Word
    print (HAMT.lookup 100 m)

