main :: IO()
main = do
    print $ countPalindromes 5 6 == 0
    print $ countPalindromes 13 5 == 5
    print $ countPalindromes 5 13 == 5

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



countPalindromes :: Int -> Int -> Int
countPalindromes number1 number2
 | abs(number1 - number2) <= 1 = 0
 | number1 < number2 = helper (number1 + 1) (number2 - 1)
 | number1 > number2 = helper (number2 + 1) (number1 - 1)
  where 
      helper :: Int -> Int -> Int
      helper currentNumber max
       | currentNumber == max = if isPalindrome currentNumber then 1 else 0
       | isPalindrome currentNumber = 1 + helper (currentNumber + 1) max
       | otherwise = helper (currentNumber + 1) max