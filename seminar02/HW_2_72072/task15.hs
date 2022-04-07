main :: IO()
main = do
    print $ isSpecial (-131) 2 == True
    print $ isSpecial 472 2 == False
    print $ isSpecial 17197 2 == True
    print $ isSpecial 12234 3 == False
    print $ isSpecial 10113 3 == True
    print $ isSpecial 353 2 == False

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

isSpecial :: Int -> Int -> Bool
isSpecial number numOfDigits = helper (abs(number))
 where
     helper :: Int -> Bool
     helper number
      | div number ( 10 ^ numOfDigits) == 0 && isPrime number == True = True
      | isPrime (mod number ( 10 ^ numOfDigits)) = helper (div number 10)
      | otherwise = False

{-
тук  условието го разбирам по следният начин (не знам дали е верният обаче.)
 при 12337 2 проверявам дали 37, 33, 23, 12 са прости
 при 12337 3 проверявам дали 337, 233, 123 са прости
 и тн.
-}