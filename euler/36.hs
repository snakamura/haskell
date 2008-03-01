import Data.Char
import Numeric

value = sum [ x | x <- [1..1000000],
                  show x == reverse (show x),
                  showBinary x == reverse (showBinary x) ]

showBinary x = showIntAtBase 2  intToDigit x ""
