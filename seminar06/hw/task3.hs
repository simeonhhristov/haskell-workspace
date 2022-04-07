main :: IO()
main = do
    print $ normalize (4, 2) == (2, 1)
    print $ normalize (8, 4) == (2, 1)
    print $ normalize (2, 4) == (1, 2)

    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ not (areEqual (2, 5) (5, 2))
    print $ areEqual (52, 123) (52 * 3, 123 * 3)
    print $ areEqual (2, 6) (5, 15)

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, y) = (div x k, div y k)
 where
     k :: Int
     k = gcd x y

sumRats :: Rat -> Rat -> Rat
sumRats (x1, x2) (y1, y2) = normalize (x1 * y2 + y1 * x2 , x2 * y2)

multiplyRats :: Rat -> Rat -> Rat
multiplyRats (x1, x2) (y1, y2) = normalize ( x1 * y1, x2 * y2)

divideRats :: Rat -> Rat -> Rat
divideRats rat1 (x1, x2) = normalize $ multiplyRats rat1 (x2, x1)

areEqual :: Rat -> Rat -> Bool
areEqual rat1 rat2 = normalize rat1 == normalize rat2