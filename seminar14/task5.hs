main ::IO()
main = do
    print $ factorize 13 == [13]
    print $ factorize 152 == [2, 2, 2, 19]
    print $ factorize 123 == [3, 41]
    print $ factorize 2 == [2]
    print $ factorize 13 == [13]
    print $ factorize 152 == [2,2,2,19]
    print $ factorize 123 == [3,41]
    print $ factorize 1000000000000000000 == [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n
 | n < 0 = error " n was negative"
 |otherwise = helper 2
  where
      helper :: Int -> Bool
      helper currentDivisor
       | fromIntegral currentDivisor > sqrt (fromIntegral n) = True 
       | mod n currentDivisor == 0 = False
       | otherwise = helper (currentDivisor + 1)

factorize :: Int -> [Int]
factorize n = helper 2 n
 where
     helper :: Int -> Int -> [Int]
     helper currentDivisor currNumber
      | isPrime currNumber = [currNumber]
      | mod currNumber currentDivisor == 0 = [currentDivisor] ++ helper currentDivisor (div currNumber currentDivisor)
      | otherwise = helper (currentDivisor + 1) currNumber