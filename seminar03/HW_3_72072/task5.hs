main :: IO()
main = do
    print $ mySin 100 1 == 0.8414709848078965  -- n = 100, x = 1
    print $ mySin 100 0.5 == 0.479425538604203

fact :: Int -> Integer
fact n 
 | n <= 1 = 1
 | otherwise = (toInteger n) * fact (n - 1)

mySin:: Int -> Double -> Double
mySin n x
 | x == 0 = 0
 | otherwise = helper 1 0
  where
      helper :: Int -> Int -> Double
      helper currExponent index
       | currExponent >= n || currExponent == n - 1  = ((-1) ^ index) * (x ** (fromIntegral currExponent)) / (fromIntegral (fact currExponent))
       | otherwise = ((-1) ^ index) * (x ** (fromIntegral currExponent)) / (fromIntegral (fact currExponent)) + helper (currExponent + 2) (index + 1)