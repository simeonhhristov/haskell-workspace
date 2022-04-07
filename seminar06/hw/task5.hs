main :: IO()
main = do
    print $ "hello"

type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2) = 
