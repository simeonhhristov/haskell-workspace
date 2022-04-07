main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
    print $ hasIncDigits 1 == True
    print $ hasIncDigits 123 == True

hasIncDigits:: Int -> Bool
hasIncDigits number
 | number < 10  = True
 | otherwise = helper number 
  where
      helper :: Int -> Bool
      helper number
       | div number 100 == 0 = if mod number 10 >= div number 10 then True else False
       | mod number 10 >= mod (div number 10) 10 = helper (div number 10)
       | otherwise = False