main ::IO()
main = do
    print $ toBinaryIndexed t1 == t1result
    print $ toBinaryIndexed t2 == t2result

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

t1:: BTree Char
t1 = Node 'a' (Node 'b' Nil (Node 'd' Nil Nil)) (Node 'c' (Node 'f' (Node 'e' Nil Nil) Nil) Nil)

t1result:: BTree (Char,Int) 
t1result = Node ('a', 2) (Node ('b',0) Nil (Node ('d', 1) Nil Nil)) (Node ('c', 5) (Node ('f', 4) (Node ('e', 3) Nil Nil) Nil) Nil)
    
t2::(Num a) => BTree a
t2 = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

t2result :: (Num a) => BTree (a, Int)
t2result = Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))

traverseDFS :: BTree a -> [(a, Int)]
traverseDFS Nil = []
traverseDFS tree = zip (helper tree) [0..]
 where
     helper :: BTree a -> [a]
     helper Nil = []
     helper (Node value left right) = (helper left) ++ [value] ++ (helper right)

getIndex::(Eq a)=> a -> BTree a -> (a,Int)
getIndex node tree = head [(a,b)| (a, b) <- (traverseDFS tree), a == node] 

toBinaryIndexed ::(Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil
toBinaryIndexed tree@(Node val left right) = helper tree tree
 where
     helper :: (Eq a) => BTree a -> BTree a -> BTree (a, Int)
     helper Nil _  = Nil
     helper tree@(Node val left right) mainTree = Node (getIndex val mainTree) (helper left mainTree) (helper right mainTree)
    