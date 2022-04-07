main :: IO()
main = do
    print $ subNum 123 5123783 == True -- x = 123, y = 5123783
    print $ subNum 0 0 == True
    print $ subNum 10 101 == True
    print $ subNum 101 101 == True
    print $ subNum 10 0 == False
    print $ subNum 1253 5123783 == False
    print $ subNum 12 0 == False

digitCount :: Int -> Int
digitCount number = helper (abs(number)) 1
  where
     helper :: Int -> Int -> Int
     helper leftOver result
      | leftOver < 10 = result
      | otherwise = helper (div leftOver 10) (result + 1)

subNum :: Int -> Int -> Bool
subNum x y
 | x > y = False
 | x == y = True
 | otherwise = helper y 
  where
      helper :: Int -> Bool
      helper leftOver 
       | mod leftOver (10 ^ digitCount x) == 0 && mod leftOver (10 ^ digitCount x) /= x = False
       | x == mod leftOver (10 ^ digitCount x) = True
       | otherwise = helper (div leftOver 10) 

--Тук решението може и да е по-просто ма не ми хрумва нищо друго сега :Д