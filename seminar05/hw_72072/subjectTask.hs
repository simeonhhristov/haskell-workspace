import Data.List

main :: IO()
main = do
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] -- == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] -- == "Maths"

type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)

--foldr (+) 0 [1,2,3]

--hardestSubject :: [Record] -> Subject
hardestSubject records = foldr1 (\ x y -> if getAvg x < getAvg y then x else y ) getUniqueSubjects
   where
    func :: Subject -> [Record]
    func subject  = [(name, currentSubject, grade) | (name, currentSubject, grade) <- records, currentSubject == subject]

    getAvg :: Subject -> Note
    getAvg subject = sum grades / fromIntegral (length grades) 

     where
         grades :: [Note]
         grades = [grade | (_, currentSubject, grade) <- records, currentSubject == subject]

    getUniqueSubjects :: [Subject]
    getUniqueSubjects = nub [ subject | (_, subject, _) <- records]