main :: IO()
main = do
    print $ isPalindrome 1 == True
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ "task1 ^^^^^^^ "

    print $ countOccurences 121 1 == 2
    print $ "task2 ^^^^^^^ "

   {-} print $ fibIter 0 == 0
    print $ fibIter 1 == 1
    print $ fibIter 2 == 1
    print $ fibIter 3 == 2
    print $ fibIter 11 == 89
    print $ fibIter 110 == 43566776258854844738105-}

    print $ sumDigitsRec 12345 == 15
    print $ sumDigitsRec 123 == 6
    print $ "task4 ^^^^^^^"

    print $ powRec 2 5
    print $ powRec 15 3
    --print $ powIter 2 5
    --print $ powIter 15 3
    print $ "task5 ^^^^^^^"

    print $ isPrime 1 == False
    print $ isPrime 2 == True
    print $ isPrime 3 == True
    print $ isPrime 6 == False
    print $ isPrime 61 == True
    print $ "task6 ^^^^^^^"

    print $ sumDivs 0 == 0
    print $ sumDivs 1 == 1
    print $ sumDivs 6 == 12 -- 1 + 2 + 3 + 6
    print $ sumDivs 12345 == 19776
    print $ "task7 ^^^^^^^"


rev :: Int -> Int
rev n
 | n < 0 = error "n was negative"
 | otherwise = helper n 0
  where
      helper :: Int -> Int -> Int
      helper leftOver result
       | leftOver < 10 = result*10 + leftOver
       | otherwise = helper (div leftOver 10) (result * 10 + mod leftOver 10)

isPalindrome :: Int -> Bool
isPalindrome n = n == rev n

----------------------------------------------------------------------------

countOccurences :: Int -> Int -> Int
countOccurences n neededNumber
 | n < 10 = if n == neededNumber then 1 else 0
 | mod n 10 == neededNumber = 1 + countOccurences(div n 10) neededNumber
 | otherwise = countOccurences(div n 10) neededNumber

fibRec :: Int -> Int
fibRec 0 = 0
fibRec 1 = 1
fibRec i
 | i < 0 = error " n was negative"
 |otherwise  = fibRec( i- 1) + fibRec (i - 2)

{-fibIter :: Integer -> Integer
fibIter i = if i< 0 then error " n was negative" else helper 0 1 i
 where
     helper :: Integer -> Integer -> Integer ->Integer
     helper i iNext leftOver
      | leftOver == 0 = i
      | leftOver == 1 = iNext
      | otherwise = helper iNext (i + iNext) (leftOver - 1)
      -}

sumDigitsRec :: Int -> Int
sumDigitsRec n
 | n < 0 = error "n was negative"
 | n < 10 = n
 | otherwise = mod n 10  + sumDigitsRec (div n 10)

powRec :: Double -> Int -> Double
powRec x n
 | n < 1 = error " n was negative"
 | n == 1 = x
 | otherwise = x * powRec x (n-1)

powIter :: Double -> Int -> Double
powIter x n
 | n < 1 = error " n was negative"
 | otherwise = helper n x
  where
      helper :: Int -> Double -> Double
      helper 1 result = result
      helper leftOver result = helper (leftOver - 1) (result * x)

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

sumDivs :: Int -> Int
sumDivs 0 = 0
sumDivs number
 |number < 0 = error " number was negative"
 |otherwise = helper 1
  where
      helper :: Int -> Int
      helper currentDivisor
       | currentDivisor == number = currentDivisor
       | mod number currentDivisor == 0 = currentDivisor + helper(currentDivisor +1 )
       | otherwise = helper(currentDivisor + 1)
       
