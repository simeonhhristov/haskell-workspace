main :: IO()
main = do
    print $ isSquare 1 == True
    print $ isSquare 2 == False
    print $ isSquare 4 == True
    print $ isSquare 17 == False
    print $ isSquare 256 == True
    print $ isSquare 2500 == True

isSquare :: Int -> Bool
isSquare 0 = False --тук пиша false no не знам по какъв начин го интерпретираш ти :Д, за мен 0 не се брои ( но ако се брои, helper трябва да започва от 0 и ще работи)
isSquare number
 | number < 0 = error " number was negative"
 | otherwise = helper 1
 where
     helper:: Int -> Bool
     helper currentNumber
      | currentNumber ^ 2 == number = True
      | currentNumber ^ 2 > number = False
      | otherwise = helper ( currentNumber + 1)