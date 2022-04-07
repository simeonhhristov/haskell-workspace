import Data.List
import Data.Char

main :: IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)
    
checkNumber :: Int -> (Int, Int)
checkNumber number = (sum $  [x | (x,y) <- zip (map digitToInt (show number)) [1 .. ], mod y 2 == 1], sum $ [x | (x,y) <- zip (map digitToInt (show number)) [1 .. ], mod y 2 == 0])