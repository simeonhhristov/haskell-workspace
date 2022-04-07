main ::IO()
main  =do 
    print $ maxDepthBlueNode colorTree  == 3

data Color = Red | Green | Blue
 deriving(Show, Eq)
data Tree = Empty | Node Color Tree Tree
 deriving(Show)

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode tree = helper 1 0
 where
     helper :: Int -> Int ->Int
     helper k foundAt
      | getLevel tree k == [] = foundAt
      | elem Blue (getLevel tree k) = helper (k + 1) k
      | otherwise = helper (k + 1) foundAt

getLevel :: Tree -> Int -> [Color]
getLevel Empty _ = []
getLevel (Node color left right) k
 | k == 1 = [color]
 | otherwise = (getLevel left (k - 1)) ++ (getLevel right (k - 1))