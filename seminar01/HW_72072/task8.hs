main :: IO()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1

--Използвам double понеже стойностите може да бъдат реални числа
snail :: Double -> Double -> Double -> Int
snail columnHeight crawlDistance slideDistance = helper columnHeight crawlDistance crawlDistance slideDistance
 where
     helper :: Double -> Double -> Double -> Double -> Int
     helper columnHeight coveredDistance crawlDistance slideDistance
      | columnHeight <= coveredDistance = 1
      | otherwise = 1 + helper columnHeight (coveredDistance + crawlDistance - slideDistance) crawlDistance slideDistance 
