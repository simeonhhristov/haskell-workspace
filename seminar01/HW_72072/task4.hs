main :: IO()
main = do
    print $ myGcd 5 13 == 1
    print $ myGcd 13 1235 == 13

myGcd :: Int -> Int -> Int
myGcd number1 number2
 | number1 == 0 = number2
 | number2 == 0 = number1
 | otherwise = myGcd number2 (mod number1 number2)

 