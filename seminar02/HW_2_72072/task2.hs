main :: IO()
main = do
    --print $ sumDigitsIter (-13) -- error "n was negative"
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 0 == 0

sumDigitsIter:: Int -> Int
sumDigitsIter number
 | number < 0  = error " number was negative"
 | otherwise = helper number 0
  where
      helper :: Int -> Int -> Int
      helper number result
       | number < 10 = number + result
       | otherwise = helper (div number 10) (result + (mod number 10))