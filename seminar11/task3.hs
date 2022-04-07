main :: IO()
main = do

    print $ grandchildrenIncreased t1 == False
    print $ grandchildrenIncreased t2 == True

data BTree = Empty | Node Int BTree BTree

t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))
t2 = Node 8 (Node 3 (Node 9 Empty Empty) (Node 10 Empty Empty)) (Node 10 (Node 11 Empty Empty) (Node 14 Empty Empty))


grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Empty = True
grandchildrenIncreased tree@(Node value left right) =  all (\x -> x > value) (getLevel tree 2) && (grandchildrenIncreased left) && (grandchildrenIncreased right)

getLevel :: BTree -> Int -> [Int]
getLevel Empty _ = []
getLevel (Node value left right) k
 | k < 0 = []
 | k == 0 = [value]
 | otherwise = getLevel left (k - 1) ++ getLevel right (k - 1)