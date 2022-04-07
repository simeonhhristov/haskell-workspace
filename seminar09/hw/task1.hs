main :: IO()
main = do

    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0


sumExpr :: (Double -> Double) -> [Double] -> (Double -> Double)
sumExpr f xs = helper f (zip xs [1..])
 where 
     helper :: (Double -> Double) -> [(Double, Int)] -> (Double -> Double)
     helper f list = (\ z -> foldr1 (+) [y * (f (z ^ (fromIntegral power))) | (y,power) <- list])
      