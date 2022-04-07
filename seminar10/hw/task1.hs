main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree  == 3

    print $ average numberBTree -- == 16.22
     --print $ average charBTree -- should not work
    -- работи просто не го закръбля..

    print $ sumLeaves numberBTree == 119
    -- print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree  == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree  == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

numberBTree :: (Num a) => BTree a
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

height :: BTree a -> Int
height Nil = 0;
height (Node val Nil Nil) = 1
height tree@(Node val left right) = 1 + if height right > height left then height right else height left

size :: BTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

average ::(Integral a, Num a) => BTree a -> Double
average tree = (fromIntegral (sumTree tree)) / (fromIntegral (size tree))

sumLeaves :: (Num a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node val Nil Nil) = val
sumLeaves tree@( Node val left right) = (sumLeaves left ) + (sumLeaves right)

areEqual ::(Eq a) => BTree a -> BTree a -> Bool
areEqual Nil _ = False
areEqual _ Nil = False
areEqual (Node val1 Nil Nil) (Node val2 Nil Nil) = val1 == val2
areEqual (Node val1 left1 right1) (Node val2 left2 right2)
 | val1 == val2 = (areEqual left1 left2) && (areEqual right1 right2)
 | otherwise  = False

setLevels :: (Num b ) => BTree a -> BTree (b, a) 
setLevels tree = helper 0 tree
 where
     helper ::(Num b ) => b -> BTree a -> BTree (b, a) 
     helper _ Nil = Nil
     helper currLevel (Node val Nil Nil) = Node (currLevel, val) Nil Nil
     helper currLevel (Node val left right) = (Node (currLevel, val) (helper (currLevel + 1) left) (helper (currLevel + 1) right))

mirrorTree :: BTree a -> BTree a 
mirrorTree Nil = Nil
mirrorTree (Node val Nil Nil ) = Node val Nil Nil
mirrorTree (Node val left right) = (Node val (mirrorTree right) (mirrorTree left))

