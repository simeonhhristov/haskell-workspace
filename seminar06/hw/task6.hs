import Data.List
main :: IO()
main = do

    print $ getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x ->  div x 2)] 2 == 2

getOddCompositionValue :: [(Int -> Int)] -> (Int -> Int)
getOddCompositionValue funcList = (\ z -> foldr1 (.) [ x | (x,y) <- zip funcList [1 .. 4], mod y 2 == 1] z)
