main :: IO()
main = do
    testTask1A
    testTask1B
    testTask2

testTask1A :: IO()
testTask1A = do
    print "Running test for task1 a..."
    print $ safePrimesCount 20 100 == 4
    print $ safePrimesCount 2 10 == 2
    print $ safePrimesCount 1 983 == 25
    print $ safePrimesCount 166 1892 == 28
    print $ safePrimesCount 1678 20097 == 155

testTask1B :: IO()
testTask1B = do
    print "Running test for task1 b..."
    print $ specialSum 3 20 == 2205
    print $ specialSum 5 31 == 665723
    print $ specialSum 8 10 == 545925272
    print $ specialSum 10 128 == 11135248639990

testTask2 :: IO()
testTask2 = do
    print "Running test for task2..."
    print $ validate 12345 == False
    print $ validate 891 == False
    print $ validate 123 == False
    print $ validate 2121 == True
    print $ validate 4736778291034 == True
    print $ validate 4485756008412 == True
    print $ validate 4485756008422 == False
    print $ validate 4214154976719 == True

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

--task1 a...
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

--task1 b...
specialSum :: Int -> Int -> Int
specialSum k m  = helper 2 0 0 
 where
     helper :: Int -> Int -> Int -> Int
     helper deg count result
      | count == k = result
      | isPrime deg && 2^deg - 1 > m = helper (deg + 1) (count + 1) (result + 2^deg - 1 )
      | otherwise = helper (deg + 1) count result

--task2...
countDigits :: Int -> Int
countDigits number
 | number < 0 = error "Number was negative"
 | otherwise = helper number 1
  where
      helper :: Int -> Int -> Int
      helper leftOver result
       | leftOver < 10 = result
       | otherwise = helper (div leftOver 10) (result + 1)

sumDigits:: Int -> Int
sumDigits number
 | number < 0  = error "Number was negative"
 | otherwise = helper number 0
  where
      helper :: Int -> Int -> Int
      helper leftOver result
       | leftOver < 10 = leftOver + result
       | otherwise = helper (div leftOver 10) (result + (mod leftOver 10))

validate :: Int -> Bool
validate number
 | countDigits number > 16 = error "A maximum of 16 digits is allowed"
 | otherwise =  helper 0 0 number
    where
        helper :: Int -> Int -> Int -> Bool
        helper index result leftOver
         | index >  (countDigits number)  = if mod result 10 == 0 then True else False
         | mod index 2 == 1 = helper (index + 1) (result + sumDigits ((mod leftOver 10) * 2)) (div leftOver 10)
         | otherwise = helper (index + 1) (result + (mod leftOver 10)) (div leftOver 10)