main ::IO()
main = do

    print $ iterator [3, 4, 5] (+1) == True
    print $ iterator [1, 2, 4] (+1) == False

iterator ::(Eq a) =>[a] -> (a -> a) -> Bool
iterator (x:[]) _ = True
iterator (x:y:xs) f 
 | f x == y = iterator (y:xs) f 
 | otherwise = False
