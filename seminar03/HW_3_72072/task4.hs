main :: IO()
main = do
    print $ pentagonalNumber 1 == 1
    print $ pentagonalNumber 2 == 5
    print $ pentagonalNumber 3 == 12
    print $ pentagonalNumber 4 == 22
    print $ pentagonalNumber 5 == 35
    print $ pentagonalNumber 6 == 51

pentagonalNumber :: Int -> Int
pentagonalNumber 1 = 1
pentagonalNumber number
 | number < 1 = error "Number must be positive"
 | otherwise = helper (number - 2) 5
 where
     helper :: Int -> Int -> Int
     helper dotsInLine result
      | dotsInLine == 0 = result
      | otherwise = result + helper (dotsInLine - 1) (dotsInLine * 3 + 4)