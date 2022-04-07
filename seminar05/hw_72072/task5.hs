main :: IO()
main = do
    print $ (pairCompose [(+1), (+2), (+3), (+4)]) 1 == 8

pairCompose :: [(Int -> Int)] -> (Int -> Int)
pairCompose [] = (*0)
pairCompose [x] = x
pairCompose (x:y:xs) = (\z -> (x . y) z  + pairCompose xs z) 


-- f(g(x))

-- g(x) = x + 2
-- f(x) = x *2
-- f(g(x)) = 



















--Тук забелязах че сме пропуснали случаят когато функциите са четен брой ипри последното извикване на PairCompose се подава празен списък и хвъря ерор
--Затова е малко по-различно от това което ми показа
