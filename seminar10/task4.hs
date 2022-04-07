import Data.List

main :: IO()
main = do

    print $ isBinarySearchTree t3 == True
    print $ isBinarySearchTree t4 == False
    print $ isBinarySearchTree t5 == False
    print $ isBinarySearchTree t6 == False

data BTree = Empty | Node Int (BTree) (BTree)

traverseDFS :: BTree -> [Int]
traverseDFS Empty = []
traverseDFS (Node value left right) = (traverseDFS left) ++ [value]  ++ (traverseDFS right)


isBinarySearchTree :: BTree -> Bool
isBinarySearchTree Empty = True
isBinarySearchTree t = traverseDFS t == (sort $ traverseDFS t)


t3 :: BTree
t3 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 6 (Node 4 Empty Empty) (Node 7 Empty Empty))) (Node 10 Empty (Node 14 (Node 13 Empty Empty) Empty))

t4 :: BTree
t4 = Node 8 (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 14 Empty Empty))

t5 :: BTree
t5 = Node 8 (Node 3 (Node 2 Empty Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 1 Empty Empty))

t6 :: BTree
t6 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 5 Empty Empty) Empty)