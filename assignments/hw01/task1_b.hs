main :: IO()
main = do
    print $ specialSum 3 20 == 2205
    print $ specialSum 5 31 == 665723
    print $ specialSum 8 10 == 545925272
    print $ specialSum 10 128 == 11135248639990


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

specialSum :: Int -> Int -> Int
specialSum k m  = helper 2 0 0 
 where
     helper :: Int -> Int -> Int -> Int
     helper deg count result
      | count == k = result
      | isPrime deg && 2^deg - 1 > m = helper (deg + 1) (count + 1) (result + 2^deg - 1 )
      | otherwise = helper (deg + 1) count result