import Data.Char
import Data.List

main :: IO()
main = do
    --print $ zipWith (+) [1 .. 9] [0 ..]
    --print $ zip [1, 4, 7, 8, 5, 2] [1 .. ]
    -- print $ (myPolynomial (\x -> x - 2) []) 5 == 0
    -- print $ (myPolynomial (\x -> x + 10) [3.62, 6.12, 9.45, 8.02, 5, 2]) (-5) == -356.45

    -- print $ dominates (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    -- print $ dominates (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True
    --print $ fold [1, 2, 3]

    -- print $ countOccurrences "Test" == [('e',1),('s',1),('t',2)]
    -- print $ countOccurrences "ThisIsAReallyLongWordContaingAlmostEveryCharacter" == [('a',6),('c',3),('d',1),('e',4),('g',2),('h',2),('i',3),('l',4),('m',1),('n',3),('o',4),('r',5),('s',3),('t',4),('v',1),('w',1),('y',2)]
    
    
    -- print $ splitPoints (1, 1) 5 [(1, 2), (2, 3), (10, 15), (-1, 1), (12, 14)] --([(1.0, 2.0), (2.0, 3.0), (-1.0, 1.0)], [(10.0, 15.0), (12.0, 14.0)])
    -- print $ splitPoints (10, 10) 5 [(1, 2), (2, 3), (10, 15), (-1, 1), (12, 14)] -- ([(10.0, 15.0), (12.0, 14.0)], [(1.0, 2.0), (2.0, 3.0), (-1.0, 1.0)])
    -- print $ splitPoints (0, 0) 2 [(0, 0), (1, 1), (2, 2), (0, 2)] -- ([(0.0,0.0),(1.0,1.0),(0.0,2.0)],[(2.0,2.0)])
    --print $ splitPoints (0, 0) (-1) [(0, 0), (1, 1), (2, 2), (0, 2)] -- Should give an error


    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"

myPolynomial ::(Num a, Enum a) => (a -> a) -> [a] -> (a -> a)
myPolynomial f ys = (\x -> sum [ i * f(y * x) | (y, i)<- zip ys [1 ..]])

-- myPolynomial :: (Double -> Double) -> [Double] -> (Double -> Double)
-- myPolynomial f ys = (\x -> sum $ zipWith(\ y i -> i * f(y *x)) ys [1 .. ])

dominates :: (Double -> Double) -> (Double -> Double) -> [Double] -> Bool
dominates _ _ [] = False
dominates f g xs = foldl1 (&&) $ map (\x -> abs( f x) >= abs(g x)) xs
--all (\ x -> abs(f x) >= abs(g x)) xs
{-
foldr (+) 0 [1, 2, 3]
3 + 0 => 3 
2 + 3 => 5 tuk 3 idva ot gorniq red
1 + 5 => 6

foldl (*) 1 [1, 2, 3]
1 * 1 => 1
1 * 2 => 2
2 * 3 => 6

foldl1 (*) [1, 2, 3]
1 * 2 => 2
2 * 3 => 6

foldr1 (*) [1, 2, 3]
2 * 3 =>6
1 * 6 => 6
-}

countOccurrences :: String -> [(Char, Int)]
countOccurrences xs = map (\ ys -> (head ys, length ys )) $ group $ sort $ map toLower xs

type Point = (Double, Double)
splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints (px, py) r ps 
 | r <= 0 = error "r was negative"   
 | otherwise = partition (\ (x, y) -> (x + px)^2 + (y - py)^2 <= r ^ 2) ps


type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)

hardestSubject :: [Record] -> Subject
hardestSubject xs = foldr1 (\ s1 s2 -> if getAvg s1 < getAvg s2 then s1 else s2) getUniqueSubjects
 where
     getUniqueSubjects :: [Subject]
     getUniqueSubjects = nub [ subject| ( _, subject, _) <- xs]
     
     getAvg :: Subject -> Note
     getAvg subject = (sum grades) / (fromIntegral $ length grades)
      where
          grades :: [Note]
          grades = [ currentGrade | (_, currentSubject, currentGrade) <- xs, currentSubject ==subject]