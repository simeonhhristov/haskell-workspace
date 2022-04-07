main :: IO()
main = do 
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5

sqAvg :: Int -> Int -> Double
sqAvg number1 number2 = (fromIntegral $ number1 ^ 2 + number2 ^ 2) / 2