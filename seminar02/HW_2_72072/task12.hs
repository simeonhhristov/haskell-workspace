main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not
    
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n
 | n < 0 = error " n was negative"
 |otherwise = helper 2
  where
      helper :: Int -> Bool
      helper currentDivisor
       | currentDivisor == n = True
       | mod n currentDivisor == 0 = False
       | otherwise = helper (currentDivisor + 1)

truncatablePrime :: Int -> Bool
truncatablePrime 1 = False
truncatablePrime number
 | number <= 0 = error "number was not positive"
 | otherwise = helper number
  where
      helper :: Int -> Bool
      helper number
       | number < 10 = isPrime number
       | isPrime number = helper (div number 10)
       | otherwise = False 
