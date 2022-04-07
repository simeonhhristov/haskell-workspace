import Data.Char

main :: IO()
main = do
    print $ hasElementsPM [] == False
    print $ hasElementsPM [1, 2, 3] == True
    print $ hasElementsFunc [] == False
    print $ hasElementsFunc [1, 2, 3] == True

    print $ "-----------------------------------"

    print $ myLengthRecNonPM [] == 0
    print $ myLengthRecNonPM [1, 2, 3] == 3
    print $ myLengthRecPM [] == 0
    print $ myLengthRecPM [1, 2, 3] == 3
    --print $ myLengthFunc [] == 0
    --print $ myLengthFunc [1, 2, 3] == 3

    print $ "-----------------------------------1"

    print $ getClosedInteval 1 9 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ getClosedInteval 9 1 == [1, 2, 3, 4, 5, 6, 7, 8, 9]


    print $ "-----------------------------------2"

    print $ inside 1 5 4 == True -- a = 1, b = 5, x = 4
    print $ inside 5 1 4 == True
    print $ inside 10 50 20 == True
    print $ inside 10 50 1 == False

    print $ "-----------------------------------3"

    print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

    print $ "-----------------------------------4"

    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

    print $ "-----------------------------------5"

    print $ incrementByLC 5 [5] == [10]
    print $ incrementByLC 4 [4, 4] == [8, 8]
    print $ incrementByLC 5 [1] == [6]
    print $ incrementByLC 5 [5, 1, 5, 3, 5] == [10, 6, 10, 8, 10]
    print $ incrementByLC 3 [5, 1, 5, 3, 5] == [8, 4, 8, 6, 8]
    print $ incrementByHOF 5 [5] == [10]
    print $ incrementByHOF 4 [4, 4] == [8, 8]
    print $ incrementByHOF 5 [1] == [6]
    print $ incrementByHOF 5 [5, 1, 5, 3, 5] == [10, 6, 10, 8, 10]
    print $ incrementByHOF 3 [5, 1, 5, 3, 5] == [8, 4, 8, 6, 8]

    print $ "-----------------------------------6"
    print $ rev 123 == 321
    print $ isPrime 5 == True
    print $ isPrime 6 == False
    print $ isPrime 11 == True
    print $ isPrime 13 == True
    print $ sumDig 142500 == 12
    print $ sumDivs 161 == 192

    print $ "-----------------------------------7"

    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]
    --print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    --print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [ i | i <- [min x y .. max x y], isPrime i && any (\ char -> char == '7') (show i)]


rev :: Int -> Int
rev n  = read $ reverse $ show $ n

isPrime :: Int -> Bool
isPrime n = n >1 && all(\ x -> mod n x /= 0) [2 .. n - 1]

sumDig :: Int -> Int
sumDig n = sum $  map digitToInt $ show n

sumDivs :: Int -> Int
sumDivs n = n + (sum $ filter (\ x -> mod n x == 0) [1 .. div n 2 + 1])

incrementByLC :: Int -> [Int] -> [Int]
incrementByLC d xs = [ x + d | x <- xs]

incrementByHOF :: Int -> [Int] -> [Int]
incrementByHOF d xs = map (\ x -> x + d ) xs -- map (+d) xs


removeAll :: Int -> [Int] ->[Int]
removeAll d [] = []
removeAll d (x:xs)
 | d == x = removeAll d xs
 | otherwise = x : removeAll d xs

removeAllHFO ::  Int -> [Int] ->[Int]
removeAllHFO d xs = filter (\ x -> x /= d) xs

removeFirst :: Int -> [Int] -> [Int]
removeFirst _ [] = []
removeFirst d (x:xs)
 | d == x = xs
 | otherwise = x : removeFirst d xs

inside :: Int -> Int -> Int -> Bool
inside x y n = elem n [min x y .. max x y]


getClosedInteval :: Int -> Int -> [Int]
getClosedInteval x y = [ min x y .. max x y]

myLengthRecNonPM :: [Int] -> Int
myLengthRecNonPM xs
 | xs == [] = 0
 | otherwise = 1 + myLengthRecNonPM (tail xs)

myLengthRecPM :: [Int] -> Int
myLengthRecPM [] = 0
myLengthRecPM (x:xs) = 1 + myLengthRecPM xs






hasElementsPM :: [Int] -> Bool
hasElementsPM [] = False
hasElementsPM _ = True -- hasElementsPM xs = True

hasElementsFunc :: [Int] -> Bool
hasElementsFunc xs = not (null xs) -- length xs/= 0