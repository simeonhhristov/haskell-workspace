main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6

reverseOrdSuff :: Int -> Int
reverseOrdSuff number = helper (abs(number)) 0
 where
     helper :: Int -> Int -> Int
     helper number suffix
      | number < 10 && number > (mod suffix 10) = suffix * 10 + number
      | mod number 10 >  (mod suffix 10) = helper (div number 10) (suffix * 10  + (mod number 10))
      | otherwise = suffix