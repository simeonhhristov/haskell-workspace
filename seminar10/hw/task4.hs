main :: IO()
main = do

    print $ isGraceful t1 -- == True
    --print $ isGraceful t2 == False

data Ntree = Nil | Node Int [Ntree]
 deriving(Show)

t1:: Ntree
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: Ntree
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]

isGraceful :: Ntree -> Bool
isGraceful tree = helper 0 tree
 where
     helper :: Int -> Bool
     helper k tree
      | getLevel tree k == [] = True 
      | all (\x -> even x) $ map (\(Node succ1 rest) -> val - succ1) succ

getLevel:: Ntree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node v succ) k
 | k == 0 = [v] -- v : []
 | otherwise = concatMap (\ x -> getLevel x (k - 1)) succ
