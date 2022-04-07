main :: IO()
main = do
    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000

    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000

sumCubesPow :: Int -> Int -> Int
sumCubesPow number1 number2 = (number1 ^ 3) + (number2 ^ 3)

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow number1 number2 = number1 * number1 * number1 + number2 * number2 * number2
