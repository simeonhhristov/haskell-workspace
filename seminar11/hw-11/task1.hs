main ::IO()
main = do
    print $ (myPolly [2.7, 3.0 .. ]) 2.2 3 == -0.4399999999999998

myPolly :: [Double] -> (Double -> Double -> Double)
myPolly xs =  (\x y -> foldr1 (*) [ (x - a) | (a, b)<- zip xs [1 .. y]] )  