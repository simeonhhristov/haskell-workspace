main :: IO()
main = do
    print $ sumDivs (-1)
    print $ sumDivs (-201)
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    print $ areAmicable (-220) 284 == True

sumDivs :: Int -> Int
sumDivs  number
 | number == 0  = 0
 | number == 1 || number == (-1) = 1
 | otherwise = helper 1
  where
      helper :: Int -> Int
      helper currentDivisor
       | currentDivisor == abs (div number 2) = if mod number currentDivisor == 0 then  currentDivisor + (abs (number)) else abs(number)
       | mod number currentDivisor == 0 = currentDivisor + helper(currentDivisor + 1)
       | otherwise = helper (currentDivisor + 1)

areAmicable :: Int -> Int -> Bool
areAmicable number1 number2
 | number1 == number2 = True
 | sumDivs number1 == sumDivs number2 = True
 | otherwise = False
-- понеже не е опоменато дали трябва да е само за неотрицателни числа го направих за всички числа използвайки abs, дано не е проблем :D
