main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10

--Използвам double понеже стойностите може да бъдат реални числа
growingPlant :: Double -> Double -> Double-> Int
growingPlant upSpeed downSpeed desiredHeight = helper upSpeed upSpeed downSpeed desiredHeight
 where
     helper :: Double -> Double -> Double -> Double -> Int
     helper currentHeight upSpeed downSpeed desiredHeight 
      | desiredHeight <= currentHeight = 1
      | otherwise = 1 + helper (currentHeight + upSpeed - downSpeed ) upSpeed downSpeed desiredHeight
