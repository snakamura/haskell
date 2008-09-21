import Control.Monad
import Data.List

value = sum $ nub values

values = do x <- [1..49]
            y <- [x + 1..2000]
            let sx = show x
                sy = show y
            guard $ notElem '0' sx
            guard $ notElem '0' sy
            guard $ length (sx ++ sy) < 6
            let p = x*y
                sp = show p
                s = sx ++ sy ++ sp
            guard $ (length sx + length sy + length sp) == 9
            guard $ notElem '0' s
            guard $ length (nub s) == 9
            return p
