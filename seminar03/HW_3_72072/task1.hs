main :: IO()
main = do
    print $ digital_root 16 == 7
    print $ digital_root 942 == 6
    print $ digital_root 132189 == 6
    print $ digital_root 493193 == 2

    print $ sumOfDigits 132189


sumOfDigits :: Int -> Int
sumOfDigits number = helper (div number 10) (mod number 10)
 where
     helper :: Int -> Int -> Int
     helper leftOver result
      | leftOver < 10 = leftOver + result
      | otherwise = result + helper (div leftOver 10) (mod leftOver 10)

digital_root :: Int -> Int
digital_root number
 | number < 0 = error "Number was negative"
 | number < 10 = number
 | otherwise = digital_root (sumOfDigits number)


    
    