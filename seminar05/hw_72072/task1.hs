main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

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

getPalindromes :: Int -> Int
getPalindromes n = helper 2 0 0
 where
     helper:: Int-> Int ->Int -> Int
     helper currentDivisor smallestPalindrome biggestPalindrome
      | currentDivisor == (div n 2) + 1 = if isPalindrome n then n + smallestPalindrome else smallestPalindrome + biggestPalindrome
      | mod n currentDivisor == 0 && isPalindrome currentDivisor && smallestPalindrome /= 0 = helper (currentDivisor + 1) smallestPalindrome currentDivisor
      | mod n currentDivisor == 0 && isPalindrome currentDivisor = helper (currentDivisor + 1) currentDivisor currentDivisor
      | otherwise = helper (currentDivisor + 1) smallestPalindrome biggestPalindrome