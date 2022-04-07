main ::IO()
main = do
    print $ removeEveryKth 3 [1 .. 9] == [1,2,4,5,7,8]
    print $ removeEveryKth 4 [1, 2, 3, 4, 5, 6, 7] == [1, 2 , 3, 5, 6, 7]

removeEveryKth :: Int -> [a] ->[a]
removeEveryKth _ [] = []
removeEveryKth n xs =[a | (a, b) <- zip xs [1 .. ], mod b n /= 0 ] 