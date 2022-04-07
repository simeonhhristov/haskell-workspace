main ::IO()
main = do

    print $ closestToAverage [ (Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)]

data Measuring = Temp {day:: Int, temperature::Float}
 deriving(Show)


closestToAverage :: [Measuring] -> Int
closestToAverage list = day $ foldr1 (\x@(Temp day1 temp1) y@(Temp day2 temp2) -> if abs( avg - temp1) < abs ( avg - temp2) then x else y) list 
 where
     avg :: Float
     avg = (sum [ temperature | (Temp day temperature)<- list]) / (fromIntegral (length list))