import Data.List

main :: IO()
main = do
    -- print $ (myLambda (\ x -> x)) 5 == 5
    -- print $ (myLambda (\ x -> x)) "Hello" == "Hello"
    -- print $ (myLambda (+1)) 5 == 6
    -- print $ (negatePred (\x -> mod x 2 == 0)) 2 == False
    -- print $ (compose (\x -> x - 5) (\y -> y + 25)) 5 == 25
    -- print $ (compose group sort) "Hello World" == [" ","H","W","d","e","lll","oo","r"]
    -- print $ (partiallyApply (\x y -> 10 * x + y) 5) 10 == 60
    
    -- print $ (difference (\x -> 2 * x)) 15.16 15.20 == 7.99999999999983e-2
    -- print $ (difference (\x -> 2 * x)) 8.5 8 == -1.0
    
    -- print $ (upperBound (*2) 100) 50 == 100
    -- print $ (upperBound (*2) 100.236) 500.002 == 100.004
    -- print $ (upperBound (\x -> x) 1.001) 1.001 == 1.001

    -- print $ sumTuple (4, 5) == 9
    -- print $ sumTuple (-5, 5) == 0
    -- print $ (\ (x, y) -> x + y) (5,6) == 11

    -- print $ dividePM (10, 5) == (2, 0) -- 10 / 5 = 2 and 10 % 5 = 0
    -- print $ dividePM (5, 10) == (0, 5) -- 5 / 10 = 0 and 5 % 10 = 5
    -- print $ divideNonPM (10, 5) == (2, 0)
    -- print $ divideNonPM (5, 10) == (0, 5)
    -- print $ (\x -> (div (fst x) (snd x), mod (fst x) (snd x)) ) (10, 5) == (2, 0)

    -- print $ normalize (4, 2) == (2, 1)
    -- print $ normalize (8, 4) == (2, 1)
    -- print $ normalize (2, 4) == (1, 2)

    

    --print $ getSquares 0 100 10 == [(0, 0), (10, 100), (20, 400), (30, 900), (40, 1600), (50, 2500), (60, 3600), (70, 4900), (80, 6400), (90, 8100), (100, 10000)]

    print $ sumVectors (1, 2, 3) (4, 5, 6) == (5, 7, 9)
    print $ sumVectors (0, 0, 0) (100, 200, -300) == (100, 200, -300)

    print $ scaleVector (1, 2, 3) 5 == (5, 10, 15)
    print $ scaleVector (5, 2, 159) (-2) == (-10, -4, -318)

type Vector = (Int, Int, Int)
sumVectors :: Vector -> Vector -> Vector
sumVectors (x1, x2, x3) (y1, y2, y3) = (x1 + y1, x2 + y2, x3 + y3)

scaleVector :: Vector -> Int -> Vector
scaleVector (x1, x2, x3) c =(x1 * c, x2 * c, x3 * c)

getSquares :: Int -> Int -> Int -> [(Int, Int)]
getSquares x y k = [(z, z * z) | z  <- [x, k .. y]]

type Rat = (Int, Int)
normalize :: Rat -> Rat
normalize (x, y) = (div x k, div y k)
 where
     k :: Int
     k = gcd x y

type Point = (Int, Int)

divideNonPM :: Point -> Point
divideNonPM x =(div (fst x) (snd x), mod (fst x) (snd x))

dividePM :: Point -> Point
dividePM (x, y) = (div x y, mod x y)

sumTuple :: (Int, Int)  -> Int
sumTuple x = fst x + snd x

upperBound :: (Ord a) => (a -> a) -> a -> (a -> a)
upperBound f y = (\ x -> max (f x) y)

difference :: (Num a) => (a -> a) -> (a -> a -> a)
difference f = (\ x y -> (f y) - (f x))


myLambda :: (a -> b) -> (a -> b)
myLambda f = f --(\x -> f x)

negatePred :: (a -> Bool) -> (a -> Bool)
negatePred p = not . p --(\x -> not p x) -- tochkata znachi kompoziciq na not i p

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = f . g --(\x -> f (g x))

-- partiallyApply :: (a -> b -> c) -> a -> b -> (b -> c) 
-- partiallyApply f x = f x --(\ y -> f x y)
