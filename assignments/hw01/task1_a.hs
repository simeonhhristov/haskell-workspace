main :: IO()
main = do
    print $ safePrimesCount 20 100 == 4
    print $ safePrimesCount 2 10 == 2
    print $ safePrimesCount 1 983 == 25
    print $ safePrimesCount 166 1892 == 28
    print $ safePrimesCount 1678 20097 == 155
    --print $ isPrime 8191


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

safePrimesCount :: Int -> Int -> Int
safePrimesCount number1 number2
 | mod (min number1 number2) 2 /= 0 = helper (min number1 number2) (max number1 number2) 0
 | otherwise = helper ((min number1 number2) + 1) (max number1 number2) 0
  where
      helper :: Int -> Int -> Int -> Int
      helper currentNumber max count
       | currentNumber >= max = if isPrime max && isPrime (div (max - 1) 2) then count + 1 else count
       | isPrime currentNumber && isPrime (div (currentNumber - 1) 2) = helper (currentNumber + 2) max (count + 1)
       | otherwise = helper (currentNumber + 2) max count