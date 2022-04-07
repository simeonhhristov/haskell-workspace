main :: IO()
main = do
    print $ removeFistOccurrence 15365 7 == 15365
    print $ removeFistOccurrence 15360 0 == 1536
    print $ removeFistOccurrence 15300 0 == 1530
    print $ removeFistOccurrence 15365 1 == 5365
    print $ removeFistOccurrence 35365 3 == 3565
    print $ removeFistOccurrence 1212 1 == 122
    print $ removeFistOccurrence 1212 2 == 121
    print $ removeFistOccurrence (removeFistOccurrence 1212 1) 1 == 22

removeFistOccurrence ::  Int -> Int -> Int
removeFistOccurrence baseNumber removeDigit
 | baseNumber < 10 = if removeDigit == baseNumber then 0 else baseNumber
 | otherwise = helper baseNumber 0 0
  where
      helper :: Int -> Int -> Int -> Int
      helper leftOver index result
       | leftOver < 10 = if removeDigit == leftOver then result else leftOver * 10 ^ index + result
       | mod leftOver 10 == removeDigit = (div leftOver 10) * 10 ^ index + result
       | otherwise = helper(div leftOver 10) (index + 1) (result + (mod leftOver 10) * 10 ^ index)

-- това което разбрах от условието на 6та е да приложа решението което предложих по време на упражнението :Д
-- проблема с решението от семинара мисля че идваше от начина по който беше дефиниран корнър кейса с x 0 на combine функцията
-- но тук нямаме такъв проблем и използваме само 1 функция :D
