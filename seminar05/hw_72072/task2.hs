main :: IO()
main = do
    print $ isArithmentic [3] == True
    print $ isArithmentic [3, 5] == True
    print $ isArithmentic [1, 2, 3, 4, 5] == True
    print $ isArithmentic [3, 5, 7, 9, 11] == True
    print $ isArithmentic [3, 5, 8, 9, 11] == False -- Тук това е вярно? мисля че просто си объркал :Д

isArithmentic:: [Int] -> Bool
isArithmentic [] = True
isArithmentic [x] = True
isArithmentic (x : xs) = x <= head xs && isArithmentic xs

--Тук създавам опашкова рекурсия, може би решението би било по ефективно ако дефинирам нова функция която да следи дали x <= head xs като параметър..
--оставям го така понеже е кратко :D