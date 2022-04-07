import Data.List ()

main :: IO ()
main = do
  testTask1
  testTask2

testTask1 :: IO ()
testTask1 = do
  print "Running test for task1..."
  print $ countRats ")1)1)1)1 P" == 0
  print $ countRats "P 1( 1( )1 1(" == 1
  print $ countRats "  P 1( 1(    )1 1(" == 1
  print $ countRats ")1)1)1)1P)1)11(" == 2
  print $ countRats "1()1)1)11(1()1)1P)11()1)1)11(1(1(1(" == 7

testTask2 :: IO ()
testTask2 = do
  print "Running test for task2..."
  print $ josephus [1, 2, 3, 4, 5, 6, 7] 3 == [3, 6, 2, 7, 5, 1, 4]
  --print $ josephus [1, 2, 3, 4, 5, 6, 7] (-1)
  print $ josephus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 1 == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  print $ josephus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2 == [2, 4, 6, 8, 10, 3, 7, 1, 9, 5]
  print $ josephus "fpFMIsu" 4 == "MfsIuFp"

countRats :: String -> Int
countRats xs = helper [x | x <- xs, x /= '1', x /= ' '] False 0 -- реших да премахна единиците и интервалите понеже те не оказват влияние, а просто усложняват задачата
  where
    helper :: String -> Bool -> Int -> Int
    helper (y : ys) hunterFound ratCount
      | null ys = if (y == '(' && not hunterFound) || (y == ')' && hunterFound) then ratCount + 1 else ratCount
      | y == '(' && not hunterFound = helper ys hunterFound (ratCount + 1)
      | y == ')' && hunterFound = helper ys hunterFound (ratCount + 1)
      | y == 'P' = helper ys True ratCount
      | otherwise = helper ys hunterFound ratCount

--josephus :: (Eq a) => [a] -> (Int -> [a])
josephus xs = \z -> case () of
  _
    | z < 0 -> error "Period was not a natural number"
    | otherwise -> helper xs [] z z
  where
    helper :: (Eq a) => [a] -> [a] -> Int -> Int -> [a]
    helper xs resultList period currentIndex
      | null xs = reverse resultList
      | currentIndex > length xs = helper xs resultList period (currentIndex - length xs)
      | otherwise = helper [x | x <- xs, x /= xs !! (currentIndex - 1)] ((xs !! (currentIndex - 1)) : resultList) period (currentIndex + period - 1)