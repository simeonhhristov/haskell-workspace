main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789

rev :: Int -> Int
rev number = helper number 0
 where
     helper :: Int -> Int -> Int
     helper number reversed
      | number < 0  = error " Number is Negative"
      | number == 0 = reversed
      | otherwise = helper(div number 10) (reversed * 10 + mod number 10)