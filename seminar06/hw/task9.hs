import Data.Char
import Data.List
main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAca" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

reduceStr :: String -> String
reduceStr sentence = helper sentence []  sentence
 where
     helper :: String -> String -> String -> String
     helper _  resultSentence (x:y:[]) = if toUpper x == toUpper y  then resultSentence else resultSentence ++ [x,y]
     helper (x:y:xs) resultSentence leftOver   
      | toUpper x == toUpper y = helper (resultSentence ++ xs) ("") xs
      | otherwise = helper (y:xs) (reverse(x : (reverse resultSentence))) (y:xs)
--в твоят пример мисля че си забравил да премахнеш последното DD