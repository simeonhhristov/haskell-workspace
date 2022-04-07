main :: IO()
main = do
    print  $ (switchsum (\ x -> x + 1) (\ x -> x * 2) 2 ) 2 == 9
    print  $ (switchsum (\ x -> x + 1) (\ x -> x * 2) 3 ) 2 == 16
    print  $ (switchsum (\ x -> x + 1) (\ x -> x * 2) 4 ) 2 == 30
    print $ foldl (-) 54 [10, 11, 26, 7]

switchsum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchsum f g n = helper f  0
 where 
     helper :: (Int -> Int) -> Int -> (Int -> Int)
     helper composed index
      | index == n = (*0)
      | mod index 2 == 0  = (\z -> composed z + helper (g . composed) (index + 1) z)
      | mod index 2 /= 0 = (\z -> composed z + helper (f . composed) (index + 1) z)
