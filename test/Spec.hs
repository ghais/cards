
import Data.Array.Unboxed as Array

x :: Array.UArray (Int, Int, Int) Int
x = Array.listArray ((0,0,0), (2,2,2)) [1..27]
main :: IO ()
main = putStrLn "Test suite not yet implemented"
