import Data.List
main ::IO()
main = do
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)]  == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)

data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq)

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)

distance :: (Floating a, RealFrac a) => Point a -> Point a -> a
distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) =  sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1)
distance _ _ = error "Points mnust be with equal coordinates"

getClosestDistance ::(RealFrac a, Floating a) => [Point a] -> (a, Point a, Point a)
getClosestDistance list = foldr1 (\ x@(dist1, x1, y1) y@(dist2, x2, y2) -> if dist1 > dist2 then y else x) [ (distance point1 point2, point1, point2)   | (point1, point2) <- map tuplify $ filter (\x -> length x == 2) $ subsequences list]

--това работи но дефиницията на getClosestDistance  ми връща (a, point a, point a), а не (double, point a, point a) даваше ми мн ерори които не можах да оправя, затова го оставям така..