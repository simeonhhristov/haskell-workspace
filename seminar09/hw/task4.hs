main ::IO()
main = do
    print $ findUncles t 5  == [3,4]
    print $ findUncles t 7  == [2,4]
    print $ findUncles t 10  == [5]

type Tree = [(Int, [Int])]

t :: Tree
t = [(1, [2, 3, 4]), (2, [5, 6]), (3, [7]), (4, [8, 9]), (5, []), (6, [10]), (7, []), (8, []), (9, []), (10, [])]

findUncles :: Tree -> Int -> [Int]
findUncles t node  = filter (\x -> x/= (fst $ head  [(dad, children ) | (dad, children) <- t, elem node children])) $ concat [hisSons | (grandpa, hisSons) <- t, elem (fst $ head  [(dad, children ) | (dad, children) <- t, elem node children]) hisSons] 
