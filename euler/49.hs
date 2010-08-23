import Data.List
import qualified Data.Set as Set
import Prime

main = print value

value = take 2 [ (x, y, z) | x <- source,
                             y <- source,
                             y > x,
                             let z = y + (y - x)
                                 sx = sort $ show x
                                 sy = sort $ show y
                                 sz = sort $ show z,
                             z < 10000,
                             Set.member z sourceSet,
                             sx == sy,
                             sy == sz]

source = takeWhile (< 10000) $ dropWhile (< 1000) primes

sourceSet = Set.fromList source
