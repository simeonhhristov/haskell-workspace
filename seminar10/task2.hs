
main :: IO()
main = do
    print $ constructMaxBTree[3, 2, 1, 6, 0, 5] == t

    print $ insert numberBTree 13 == (Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil)))
    print $ (numberBTree == numberTreeBefore) == True
    print $ secondNumberTree == Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)
numberBTree :: (Num a) => BTree al
numberBTree = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

t ::(Num a) => BTree a
t = Node 6 (Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil)

constructMaxBTree ::(Ord a) => [a] -> BTree a
constructMaxBTree [] = Nil
constructMaxBTree xs = Node (maximum xs) (constructMaxBTree $ takeWhile (/= maximum xs) xs) (constructMaxBTree $ tail $ dropWhile (/= maximum xs) xs)

insert ::(Ord a) => BTree a -> a -> BTree a
insert Nil element = Node element Nil Nil
insert (Node value left right) element
 | element >= value = (Node value left (insert right element))
 | otherwise = (Node value (insert left element) right)

numberTreeAfter :: (Num a, Ord a) => BTree a
numberBTree =foldl1 insert Nil [10, 5 , 3, 7, 15, 18]

secondNumberTree :: (Num a, Ord a) => BTree a
secondNumberTree = foldl insert Nil [10, 5, 3, 1, 7, 6, 15, 13, 18]


