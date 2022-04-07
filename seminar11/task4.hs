main ::IO()
main = do

    print $ isSymmetric Nil == True
    print $ isSymmetric t3 == False
    print $ isSymmetric t4 == True
    print $ isSymmetric t5 == True

data BTree = Nil | Node Int BTree BTree


t3 :: BTree                     --   1
t3 = Node 1 (Node 2 Nil Nil)    --  / \
            (Node 3 Nil Nil)    -- 2   3

t4 :: BTree                             --     1
t4 = Node 1 (Node 2 (Node 3 Nil Nil)    --    / \
                    Nil)                --   2   2
            (Node 2 Nil                 --  /     \
                    (Node 3 Nil Nil))   -- 3       3

t5 :: BTree                                     --       1
t5 = Node 1 (Node 2 (Node 3 Nil Nil)            --    /     \
                    (Node 7 (Node 5 Nil Nil)    --   2       2
                            Nil))               --  / \     / \
            (Node 2 (Node 7 Nil                 -- 3   7   7   3
                            (Node 5 Nil Nil))   --    /     \
                    (Node 3 Nil Nil))

isSymmetric :: BTree -> Bool
isSymmetric Nil = True
isSymmetric tree@(Node value left right) = helper left right 0
 where
     helper :: BTree -> BTree -> Int -> Bool
     helper Nil Nil _= True
     helper Nil _ _= False
     helper _ Nil _= False
     helper left right k
      | getLevel left k == [] && getLevel right k == [] = True
      | getLevel left k == reverse (getLevel right k) = helper left right (k + 1)
      | otherwise = False

getLevel :: BTree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value left right) k
 | k < 0 = []
 | k == 0 = [value]
 | otherwise = getLevel left (k - 1) ++ getLevel right (k - 1)