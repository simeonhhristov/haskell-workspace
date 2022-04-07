import Data.List
main :: IO()
main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)] == [1, 2, 3, 4]
    print $ neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 2 == [3, 4]
    print $ neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 4 == []
    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] == [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

    print " task2 .."
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] == False
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [2, 3] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [3, 1] == False

nodes :: [(Int, Int)] -> [Int]
nodes xs = helper $ unzip xs
 where
     helper :: ([Int], [Int]) -> [Int]
     helper (x, y) = nub $  x ++ y

neighbours :: [(Int, Int)] -> Int -> [Int]
neighbours graph start = [z | (x,z) <- graph, x == start]

adjacencyList :: [(Int, Int)] -> [(Int,[Int])]
adjacencyList graph = [(start, neighbours graph start) | start <- nodes graph]

isPath :: [(Int,[Int])] -> [Int] -> Bool
isPath graph (x:xs) = elem x (nodes1 graph) && helper xs x
 where
     helper :: [Int] -> Int -> Bool
     helper [] _ = True
     helper (x:xs) currentParrent = elem x  (successors graph currentParrent ) && helper xs x

successors :: [(Int,[Int])] -> Int -> [Int]
successors [] _ = error "element not present"
successors ((parrent, succ):xs) start
    | start == parrent = succ
    | otherwise = successors xs start 

nodes1 :: [(Int, [Int])] -> [Int]
nodes1 graph = [start | (start,_) <- graph]