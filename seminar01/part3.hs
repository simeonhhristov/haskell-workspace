main :: IO()
main = do
    print $ fib 11 == 89
   -- print $ fibIter 11 == 89
   -- print $ fibIter 110 == 43566776258854844738105

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n
 | n < 0 = error "n was negative"
 | otherwise = fib (n - 1) + fib (n - 2)