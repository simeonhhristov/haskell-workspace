main :: IO()
main = do
    print $ removeD 1 111 == 0
    print $ removeD 1 656 == 656
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0

removeD :: Int -> Int -> Int
removeD digit number = helper digit number 1
 where
     helper ::Int -> Int -> Int -> Int
     helper digit number result
      | number < 10  = if number == digit then 0 else number * result
      | mod number 10 == digit = helper digit (div number 10) (result)
      | otherwise = (mod number 10) * result + helper digit (div number 10) (result * 10)
