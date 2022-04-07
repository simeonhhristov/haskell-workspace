main :: IO()
main = do

    --print $ (TwoD 5 6)

    -- print $ distance (TwoD 2 5) (TwoD 6 9) == 5.66
    -- print $ distance (ThreeD 2 5 10) (ThreeD 6 9 (-5)) == 16.03

    -- print $ closestTo (ThreeD 2 5 10) [(ThreeD 4 5 6), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == ThreeD 4.0 5.0 6.0

    print $ db
    print $ getTotal
    print $ "Buying 500 bread:"
    print $ buy "bread" 500 db
    print $ "Buying 500 water:"
    print $ buy "water" 500 (buy "bread" 500 db)
    print $ "Buying 100 soap:"
    print $ buy "soap" 100 (buy "water" 500 (buy "bread" 500 db))
    print $ "Buying 500 bread:"
    print $ buy "bread" 500 (buy "soap" 100 (buy "water" 500 (buy "bread" 500 db)))
    --print $ buy "water" 100 $ buy "bread" 500 $ buy "soap" 100 $ buy "water" 500 $ buy "bread" 500 db -- Should give an error.
    --print $ buy "soap" 200 $ buy "bread" 500 $ buy "soap" 100 $ buy "water" 500 $ buy "bread" 500 db -- Should give an error.

type Product = (String, Int, Double)
type Shop = [Product]

db :: Shop
db = [("bread", 1000, 1.20), ("milk", 2000, 4.5), ("lamb", 5000, 10), ("cheese", 750, 5), ("butter", 1000, 5.50), ("water", 500, 0.50), ("soap", 250, 4.50)]

getTotal :: Double
getTotal = sum [(fromIntegral  quantity) * price | (_,quantity, price) <- db]


buy :: String -> Int -> Shop -> Shop
buy name _ [] = []
buy name amount (x@(currentName, currentQuan, currentPrice) : xs)
 | name == currentName && amount == currentQuan = xs
 | name == currentName && amount > currentQuan = error $  " Not Enough " ++ name
 | name == currentName = (currentName, currentQuan - amount,currentPrice) : xs
 | otherwise = x : buy name amount xs

--x@ - ako ne se nujdaq ot koordinatite izpolzvam x vmesto pattern matcha deto napravihme


data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq )

-- task2 ..
roundTwo ::(Fractional a, RealFrac a) => a -> a
roundTwo x = (fromIntegral $ round $ x * 100) / 100

distance ::(Floating a, RealFrac a) => Point a -> Point a -> a
distance (TwoD x1 y1) (TwoD x2 y2) = roundTwo $ sqrt $ (x2 - x1) * (x2 -x1) + (y2 - y1)*(y2 - y1)
distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = roundTwo $ sqrt $ (x2 - x1) * (x2 -x1) + (y2 - y1)*(y2 - y1) + (z2 - z1)*(z2-z1)
distance _ _ = error " Points not in same dimension"

-- task3 ..
closestTo ::(Ord a, Floating a, RealFrac a) => Point a -> [Point a] -> Point a
closestTo p ps = foldr1 (\ p1 p2 -> if  distance p1 p < distance p2 p then p1 else p2) ps


--task4..