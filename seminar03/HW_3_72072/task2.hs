main :: IO()
main = do
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1  ==  -0.6666666666666667
    print $ calcSeriesSum 1 2  ==  -1.2000000000000002
                                -- -1.2
    print $ calcSeriesSum 1 3  == -1.047619047619048
                               -- -1.0476190476190477
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
                              -- -1.0753246753246752
    print $ calcSeriesSum 1 6 == -1.0762718762718764

calcSeriesSum :: Double -> Double -> Double
calcSeriesSum x n
 | n == 0 = (-2)
 | otherwise = helper 1 0 3 3
  where
      helper :: Double -> Int -> Double -> Double -> Double
      helper currExponent index currDivisor helpDivisor
       | currExponent == n = (-2) + (-1) ^ index * ((2 ** (currExponent + 1)) * x ** currExponent) / currDivisor
       | otherwise = (-1) ^ index * ((2 ** (currExponent + 1)) * x ** currExponent) / currDivisor + helper (currExponent + 1) (index + 1) (currDivisor * (helpDivisor  + 2)) (helpDivisor + 2)


-- Тук мисля че съм я решил, понеже съм я гледал поне час за грешки и не намирам, мисля че точността на double се разминава леко, може би ако ти си 
--използвал друг подход, но може и да греша не съм сигурен. Ако намериш нещото което я кара да се чупи ще съм благодарен :D

-- също така е късно ии това е причината поради която има променливи с неадекватни имена :D