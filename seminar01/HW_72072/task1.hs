main :: IO()
main = do
    print $ isEvenIf 2 == "Yes"
    print $ isEvenIf 15452 == "Yes"
    print $ isEvenIf 321 == "No"
    print $ isEvenGuards 2 == "Yes"
    print $ isEvenGuards 15452 == "Yes"
    print $ isEvenGuards 321 == "No"

isEvenIf :: Int -> String
isEvenIf number = if (mod number 2) == 0 then "Yes" else "No"

isEvenGuards :: Int -> String
isEvenGuards number
 | mod number 2 == 0 = "Yes"
 | otherwise  = "No"