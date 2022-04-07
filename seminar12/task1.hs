main::IO()
main = do
    --print $ containsWord t1 "" -- error
    print $ containsWord t1 "acd" == True
    print $ containsWord t1 "cd" == True
    print $ containsWord t1 "af" == False
    print $ containsWord t1 "ac" == False
    print $ containsWord t1 "acdh" == False
    print $ containsWord t2 "ab" == True
    print $ containsWord t2 "ad" == False
    print $ containsWord t3 "bdh" == True
    print $ containsWord t3 "bdi" == True
    print $ containsWord t1 "e"


data BTree = Nil | Node Char (BTree) (BTree)

t1 :: BTree 
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree 
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree 
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil)) 

getLevel :: BTree -> Int -> [Char]
getLevel Nil _ = []
getLevel (Node value left right) k
 | k == 0 = [value]
 | otherwise = getLevel left (k - 1) ++ getLevel right (k - 1)

containsWord :: BTree -> String -> Bool
containsWord Nil _ = False
containsWord _ "" = error " empty string"
containsWord (Node val Nil Nil) [x] = x==val
containsWord (Node val left right) word@(x:xs)
 | val == x = helper left xs || helper right xs
 | otherwise = containsWord left word || containsWord right word
  where
      helper :: BTree -> String ->Bool   
      helper Nil [] = False
      helper (Node value Nil Nil) [x] = x == value
      helper (Node val left right) (x:xs)
       | val ==  x = helper left xs || helper right xs
       | otherwise = False
      helper _ _ = False
-- t1:    a
--       / \
--      c   b
--     / \   \
--    f   d   e
