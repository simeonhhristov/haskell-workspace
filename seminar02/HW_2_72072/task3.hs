main :: IO()
main = do
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19 -- 2 + 3
    print $ sumPrimeDivs 45136 == 53
    print $ isPrime 3

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

sumPrimeDivs :: Int -> Int
sumPrimeDivs number
 | number == 0 = 0
 | otherwise = helper 1
  where
      helper :: Int -> Int
      helper currentDivisor
       | currentDivisor == number = if isPrime currentDivisor == True then currentDivisor else 0
       | mod number currentDivisor == 0 && isPrime currentDivisor == True = currentDivisor + helper(currentDivisor + 1)
       | otherwise = helper(currentDivisor + 1)
