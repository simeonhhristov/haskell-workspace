main :: IO()
main = do
    print $ Circle 5 == Circle 5
    print $ Rectangle 5 5
    print $ Triangle 1 5 5
    print $ Cylinder 5 10
    print $ isSummer Summer

    print $ "task4.."
    print $ myImages (\x -> x * x) (2+) [Point 2 2, Point 1 2, Point 3 7] == [Point 2 2, Point 3 7]

    print $ "task5.."
    print $ isRound (Circle 5) == True
    print $ isRound (Rectangle 2.5 4.5) == False
    print $ isRound (Rectangle 5.5 20.6) == False
    print $ isRound (Triangle 5.3 3.9 4.89) == False
    print $ isRound (Cylinder 20 30) == True

    print $ is2D (Circle 5) == True
    print $ is2D (Rectangle 2.5 4.5) == True
    print $ is2D (Rectangle 5.5 20.6) == True
    print $ is2D (Triangle 5.3 3.9 4.89) == True
    print $ is2D (Cylinder 20 30) == False



data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show,Eq)

data Season = Summer | Winter | Autumn | Spring
 deriving (Show)

isSummer :: Season -> Bool
isSummer Summer =  True
isSummer _ =  False

data Point = Point Int Int
 deriving (Show, Eq)
myImages :: (Int -> Int) -> (Int -> Int) -> [Point] -> [Point]
myImages f g xs = [ (Point x y) | (Point x y) <- xs, f x == g y]

isRound :: Shape a -> Bool
isRound (Circle _) = True
isRound (Cylinder _ _) = True
isRound _ = False

is2D :: Shape a -> Bool
is2D (Cylinder _ _ ) = False
is2D _ = True
