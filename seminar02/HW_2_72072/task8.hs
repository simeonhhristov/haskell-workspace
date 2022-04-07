main :: IO()
main = do
    print $ maxMultiple (-4) 13 == 12
    print $ maxMultiple 3 10 == 9
    print $ maxMultiple 7 17 == 14
    print $ maxMultiple 10 50 == 50
    print $ maxMultiple 37 200 == 185
    print $ maxMultiple 7 100 == 98

maxMultiple :: Int-> Int -> Int
maxMultiple 0 bound = error "Can't divide by 0"
maxMultiple 1 bound = bound
maxMultiple divisor bound
 | bound <= 0 = error " Bound is negative"
 | mod bound divisor == 0 = bound
 | otherwise = helper (bound - 1)
  where
      helper :: Int -> Int
      helper result
       | mod result divisor == 0 = result
       | otherwise = helper (result - 1)
