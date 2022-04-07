main ::IO()
main = do
    print $ minDepthGreenNode colorTree -- == 3
    print $ getLevel colorTree 2
data Color = Red | Green | Blue
 deriving(Show, Eq) 

data Tree = Empty | Node Color Tree Tree
 deriving(Show) 
colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)
 

--minDepthGreenNode :: Tree -> Int
minDepthGreenNode tree@(Node curr left right) = helper 1
 where
     helper :: Int -> Int
     helper k
      | getLevel tree k == [] = error "no green in tree"
      | elem Green (getLevel tree k) = k 
      | otherwise = helper (k + 1)

getLevel :: Tree -> Int -> [Color]
getLevel Empty _ = []
getLevel (Node color left right) k
 | k == 1 = [color]
 | otherwise = (getLevel left (k - 1)) ++ (getLevel right (k - 1))

--        Blue      lvl 1
--       /    \
--    Red      Red   lvl 2
--    /        /  
-- Green     Blue  lvl 3
--           /   \
--        Green  Red lvl 4

-- поне това разбирам от тестовият случай

