main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True -- 1 + 2 + 3 = 6 = 6
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True

isPerfect :: Integer -> Bool
isPerfect number
 | number <= 1 = False
 | otherwise = helper 1 0
  where
      helper :: Integer -> Integer -> Bool
      helper currentDivisor sumOfDivs
       | currentDivisor == div number 2 = if mod number currentDivisor == 0 &&  sumOfDivs + currentDivisor == number then True else False
       | mod number currentDivisor == 0 = helper (currentDivisor + 1) (sumOfDivs + currentDivisor)
       | otherwise = helper (currentDivisor + 1) sumOfDivs
-- Алгоритъма работи, просто се бави малко на това голямото число.
-- Не измислих друг начин за съкращаване на нужното време освен да изпълнявам алгоритъма до "половината на числото", което ми съкращава времето на половина.
