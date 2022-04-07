main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

    print $ containsDigit (-830) 8

containsDigit :: Int -> Int -> Bool
containsDigit number digit =  helper (abs(number))
 where
     helper :: Int -> Bool
     helper number
      | number < 10 = if number == digit then True else False
      | mod number 10 == digit = True
      | mod number 10 /= digit = helper (div number 10)

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

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes numberOfNumbers digit --знам че името на първата променлива не е много добро ама не измислих друго :D
 | numberOfNumbers <= 0 = error "The number of numbers must be a positive integer"
 | otherwise = helper 0 0 2
  where
      helper :: Int -> Int -> Int -> Int
      helper count sum currentNumber
       | count == numberOfNumbers = sum
       | isPrime currentNumber && containsDigit currentNumber digit = helper (count + 1) (sum + currentNumber) ( currentNumber + 1)
       | otherwise = helper (count) (sum) (currentNumber + 1)
