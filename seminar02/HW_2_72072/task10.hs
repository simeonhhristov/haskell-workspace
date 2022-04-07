main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14

everyOther :: Int -> Int
everyOther number
 | number < 10  = error " number must be >= 10"
 | otherwise = helper number 1 0
  where
      helper :: Int -> Int -> Int -> Int
      helper number index result
       | mod number 10 == 0 = if mod index 2 == 0 then  result + number else result
       | mod index 2 == 0 = helper (div number 10) (index + 1) (result * 10 + (mod number 10))
       | otherwise = helper (div number 10) (index + 1) (result)
{-
Не съм сигурен дали това е точното условие, но това което разбрах е да образувам число
от всяка втора цифра в дадено число
-}