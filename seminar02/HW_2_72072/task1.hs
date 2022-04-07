main :: IO()
main = do
    --print $ countDigitsRec (-13) -- error "n was negative"
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3
    print $ countDigitsRec 0 == 1
    
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 1
    print $ countDigitsIter 0 == 1

countDigitsRec :: Int -> Int
countDigitsRec number
 | number < 0 = error "number was negative"
 | number < 10 = 1
 | otherwise = 1 + countDigitsRec(div number 10)
 
countDigitsIter :: Int -> Int
countDigitsIter number
 | number < 0 = error "number was negative"
 | otherwise = helper number 1
  where
      helper :: Int -> Int -> Int
      helper leftOver result
       | leftOver < 10 = result
       | otherwise = helper (div leftOver 10) (result + 1)
--Понеже в условието пише неотрицателно число съм включил и 0 в допустимите стойности