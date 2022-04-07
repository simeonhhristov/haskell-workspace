main :: IO()
main = do
    print $ validate 12345 == False
    print $ validate 891 == False
    print $ validate 123 == False
    print $ validate 2121 == True
    print $ validate 4736778291034 == True
    print $ validate 4485756008412 == True
    print $ validate 4485756008422 == False
    print $ validate 4214154976719 == True

countDigits :: Int -> Int
countDigits number
 | number < 0 = error "Number was negative"
 | otherwise = helper number 1
  where
      helper :: Int -> Int -> Int
      helper leftOver result
       | leftOver < 10 = result
       | otherwise = helper (div leftOver 10) (result + 1)

sumDigits:: Int -> Int
sumDigits number
 | number < 0  = error "Number was negative"
 | otherwise = helper number 0
  where
      helper :: Int -> Int -> Int
      helper leftOver result
       | leftOver < 10 = leftOver + result
       | otherwise = helper (div leftOver 10) (result + (mod leftOver 10))

validate :: Int -> Bool
validate number
 | countDigits number > 16 = error "A maximum of 16 digits is allowed"
 | otherwise =  helper 0 0 number
    where
        helper :: Int -> Int -> Int -> Bool
        helper index result leftOver
         | index >  (countDigits number)  = if mod result 10 == 0 then True else False
         | mod index 2 == 1 = helper (index + 1) (result + sumDigits ((mod leftOver 10) * 2)) (div leftOver 10)
         | otherwise = helper (index + 1) (result + (mod leftOver 10)) (div leftOver 10)
