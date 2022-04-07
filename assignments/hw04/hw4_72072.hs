main ::IO()
main = do
    print $ "Running tests for task1.."
    task1
    print $ "Running tests for task2.."
    task2

task1::IO()
task1 = do
    print $ getFeaturedStars "MGM" 1995 db  == ["Jack Nicholson", "Sandra Bulloc"]
    print $ getFeaturedStars "USA Entertainm." 2001 db  == ["Billy Bob Thornton", "Scarlett Johansson", "Orlando Bloom", "Cate Blanchett", "Liv Tyler"]

    print $ getPresident "Paramount" db == "Calvin Coolidge"
    print $ getPresident "Fox" db == "Ted Turner"
    print $ getPresident "USA Entertainm." db == "Stephen Spielberg"

    print $ getHigherProductions "Calvin Coolidge" db  == ["Pretty Woman","The Man Who Wasn't There","Logan's run","Star Wars","Empire Strikes Back","Star Trek","The Usual Suspects","The Fellowship of the Ring"]
    print $ getHigherProductions "Stephen Spielberg" db == ["Pretty Woman","The Man Who Wasn't There","Logan's run","Star Wars","Empire Strikes Back","The Usual Suspects"]
    print $ getHigherProductions "George Lucas" db  == []

task2::IO()
task2 = do 
    print $ toBinaryIndexed t1 == t1result
    print $ toBinaryIndexed t2 == t2result

--Code for task1..
type Name = String 
type Title = String
type Address = String
type Year = Int
type Gender = Char
type Length = Int
type ProducerID = Int
type Networth = Integer

data Movie = Movie Title Year Length Name ProducerID
 deriving (Show )

data MovieStar = MovieStar Name Gender
 deriving (Show)

data StarsIn = StarsIn Name Title
 deriving (Show)

data Studio = Studio Name Int
 deriving (Show)

data MovieExec = MovieExec Name ProducerID Networth
 deriving (Show)

type MovieDB = ([Movie], [MovieStar], [StarsIn], [Studio], [MovieExec])

studios :: [Studio]
studios = [Studio "Disney" 199, Studio "USA Entertainm." 222, Studio "Fox" 333, Studio "Paramount" 123, Studio "MGM" 555]

movieExecs :: [MovieExec]
movieExecs = [MovieExec "George Lucas" 555 200000000, MovieExec "Ted Turner" 333 125000000, MovieExec "Stephen Spielberg" 222 100000000, MovieExec "Merv Griffin" 199 112000000, MovieExec "Calvin Coolidge" 123 20000000]

movies :: [Movie]
movies = [Movie "Pretty Woman" 1990 119 "Disney" 199,Movie "The Man Who Wasn't There" 2001 116 "USA Entertainm." 555,Movie "Logan's run" 1976 120 "Fox" 333,Movie "Star Wars" 1977 124 "Fox" 555,Movie "Empire Strikes Back" 1980 111 "Fox" 555,Movie "Star Trek" 1979 132 "Paramount" 222,Movie "Star Trek: Nemesis" 2002 116 "Paramount" 123,Movie "Terms of Endearment" 1983 132 "MGM" 123,Movie "The Usual Suspects" 1995 106 "MGM" 199,Movie "Gone With the Wind" 1938 238 "MGM" 123,Movie "The Fellowship of the Ring" 2001 178 "USA Entertainm." 222]

stars :: [MovieStar]
stars = [MovieStar "Jane Fonda" 'F',MovieStar "Alec Baldwin" 'M',MovieStar "Kim Basinger" 'F',MovieStar "Harrison Ford" 'M',MovieStar "Debra Winger" 'F',MovieStar "Jack Nicholson" 'M',MovieStar "Sandra Bullock" 'F',MovieStar "Orlando Bloom" 'M',MovieStar "Cate Blanchett" 'F',MovieStar "Liv Tyler" 'F',MovieStar "Billy Bob Thornton" 'M',MovieStar "Scarlett Johansson" 'F']

starsIn :: [StarsIn]
starsIn = [StarsIn "Kim Basinger" "Star Wars",StarsIn "Alec Baldwin" "Star Wars",StarsIn "Harrison Ford" "Star Wars",StarsIn "Harrison Ford" "Empire Strikes Back",StarsIn "Jack Nicholson" "The Usual Suspects",StarsIn "Jane Fonda" "Terms of Endearment",StarsIn "Jack Nicholson" "Terms of Endearment",StarsIn "Sandra Bulloc" "The Usual Suspects",StarsIn "Billy Bob Thornton" "The Man Who Wasn't There",StarsIn "Scarlett Johansson" "The Man Who Wasn't There",StarsIn "Orlando Bloom" "The Fellowship of the Ring",StarsIn "Cate Blanchett" "The Fellowship of the Ring",StarsIn "Liv Tyler" "The Fellowship of the Ring"]

db :: MovieDB
db = (movies, stars, starsIn, studios, movieExecs)

getFeaturedStars :: Name -> Int -> MovieDB -> [Name]
getFeaturedStars studioName year db@(movies, stars, starsIn, studios, movieExecs) =  [ actor | (StarsIn actor movie) <- starsIn, elem movie [movieName | (Movie movieName mYear mLength studio prodId) <- movies, studio == studioName, year == mYear]]
    
getPresident :: Name -> MovieDB -> Name
getPresident studioName db@(movies, stars, starsIn, studios, movieExecs) = head [name | (MovieExec name prodId cash) <-movieExecs, elem prodId [presidentId | (Studio name presidentId)<- studios, name == studioName]]

getHigherProductions :: Name -> MovieDB -> [Name]
getHigherProductions producer db@(movies, stars, starsIn, studios, movieExecs) = [ movieName | (Movie movieName mYear mLength studio producer) <-movies, elem producer producerIDs] 
 where
     currentProdCash :: Integer
     currentProdCash = head [ cash |(MovieExec name prodId cash) <-movieExecs, producer == name]

     producerIDs :: [Int]
     producerIDs = [prodId | (MovieExec name prodId cash) <-movieExecs, cash > currentProdCash]
    
--Code for task2.. 

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

t1:: BTree Char
t1 = Node 'a' (Node 'b' Nil (Node 'd' Nil Nil)) (Node 'c' (Node 'f' (Node 'e' Nil Nil) Nil) Nil)

t1result:: BTree (Char,Int) 
t1result = Node ('a', 2) (Node ('b',0) Nil (Node ('d', 1) Nil Nil)) (Node ('c', 5) (Node ('f', 4) (Node ('e', 3) Nil Nil) Nil) Nil)
    
t2::(Num a) => BTree a
t2 = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

t2result :: (Num a) => BTree (a, Int)
t2result = Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))

traverseDFS :: BTree a -> [(a, Int)]
traverseDFS Nil = []
traverseDFS tree = zip (helper tree) [0..]
 where
     helper :: BTree a -> [a]
     helper Nil = []
     helper (Node value left right) = (helper left) ++ [value] ++ (helper right)

getIndex::(Eq a)=> a -> BTree a -> (a,Int)
getIndex node tree = head [(a,b)| (a, b) <- (traverseDFS tree), a == node] 

toBinaryIndexed ::(Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil
toBinaryIndexed tree@(Node val left right) = helper tree tree
 where
     helper :: (Eq a) => BTree a -> BTree a -> BTree (a, Int)
     helper Nil _  = Nil
     helper tree@(Node val left right) mainTree = Node (getIndex val mainTree) (helper left mainTree) (helper right mainTree)


















