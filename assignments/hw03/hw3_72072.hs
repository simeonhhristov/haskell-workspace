main::IO()
main = do
    testTask1
    testTask2

testTask1 :: IO()
testTask1 = do
    print $ "Running test for task1.."
    print $ getMoviesLongerThan "Star Wars" db == ["Star Trek", "Terms of Endearment", "Gone With the Wind", "The Fellowship of the Ring"]
    print $ getMoviesLongerThan "The Fellowship of the Ring" db == ["Gone With the Wind"]

    print $ getMaleActorsIn "Terms of Endearment" db == ["Jack Nicholson"]
    print $ getMaleActorsIn "Star Wars" db == ["Alec Baldwin", "Harrison Ford"]

    print $ getFemaleActorsFrom 1983 db == ["Jane Fonda"]
    print $ getFemaleActorsFrom 2001 db -- == ["Cate Blanchett", "Liv Tyler","Scarlett Johansson"]
    --това работи но ги изкарва в различна подредба..

testTask2 :: IO()
testTask2 = do
    print $ "Running test for task2.."
    print $ degr t1 8  == 3
    print $ degr t1 6  == 4
    print $ degr t1 7  == 3
    print $ degr t1 18  == 1


    print $ degr t2 's'  == 1
    print $ degr t2 'k'  == 4
    print $ degr t2 '1'  == 3


--Task1..
getLengthByTitle :: Title -> MovieDB -> Int
getLengthByTitle target (movies, movieStar, starsIn) = head [length | (Movie title year length) <- movies, title == target]

getMoviesLongerThan :: Title -> MovieDB -> [Title]
getMoviesLongerThan base db@(movies, movieStar, starsIn) = [title | (Movie title year length) <- movies, length > getLengthByTitle base db]

getMaleActorsIn :: Title -> MovieDB -> [Name]
getMaleActorsIn target (movies, movieStar, starsIn) = [name | (MovieStar name gender) <- movieStar, gender == 'M', elem name [name | (StarsIn name movie) <- starsIn, movie == target]]

getMoviesByYear :: Year -> [Movie] -> [Name]
getMoviesByYear target movies = [title | (Movie title year length) <- movies, year == target]

getFemales :: [Name] -> [MovieStar] -> [Name]
getFemales allNames stars = [ x | x <- allNames, elem x [name | (MovieStar name gender) <- stars, gender == 'F']]

getFemaleActorsFrom :: Year -> MovieDB -> [Name]
getFemaleActorsFrom givenYear (movies, movieStar, starsIn) = getFemales [name | (StarsIn name title) <- starsIn, elem title (getMoviesByYear givenYear movies)] stars

type Name = String
type Title = String
type Year = Int
type Gender = Char
type Length = Int

data Movie = Movie Title Year Length
 deriving(Show)

data MovieStar = MovieStar Name Gender
 deriving(Show)

data StarsIn = StarsIn Name Title
 deriving(Show)

type MovieDB = ([Movie], [MovieStar], [StarsIn])

movies :: [Movie]
movies = [Movie "The Man Who Wasn't There" 2001 116,
        Movie "Logan's run" 1976 120,
        Movie "Star Wars" 1977 124,
        Movie "Empire Strikes Back" 1980 111,
        Movie "Star Trek" 1979 132,
        Movie "Star Trek: Nemesis" 2002 116,
        Movie "Terms of Endearment" 1983 132,
        Movie "The Usual Suspects" 1995 106,
        Movie "Gone With the Wind" 1938 238,
        Movie "The Fellowship of the Ring" 2001 178]

stars :: [MovieStar]
stars = [MovieStar "Jane Fonda" 'F',
        MovieStar "Alec Baldwin" 'M',
        MovieStar "Kim Basinger" 'F',
        MovieStar "Harrison Ford" 'M',
        MovieStar "Debra Winger" 'F',
        MovieStar "Jack Nicholson" 'M',
        MovieStar "Sandra Bullock" 'F',
        MovieStar "Orlando Bloom" 'M',
        MovieStar "Cate Blanchett" 'F',
        MovieStar "Liv Tyler" 'F',
        MovieStar "Billy Bob Thornton" 'M',
        MovieStar "Scarlett Johansson" 'F']

starsIn :: [StarsIn]
starsIn = [StarsIn "Kim Basinger" "Star Wars",
        StarsIn "Alec Baldwin" "Star Wars",
        StarsIn "Harrison Ford" "Star Wars",
        StarsIn "Harrison Ford" "Empire Strikes Back",
        StarsIn "Jack Nicholson" "The Usual Suspects",
        StarsIn "Jane Fonda" "Terms of Endearment",
        StarsIn "Jack Nicholson" "Terms of Endearment",
        StarsIn "Sandra Bulloc" "The Usual Suspects",
        StarsIn "Billy Bob Thornton" "The Man Who Wasn't There",
        StarsIn "Scarlett Johansson" "The Man Who Wasn't There",
        StarsIn "Orlando Bloom" "The Fellowship of the Ring",
        StarsIn "Cate Blanchett" "The Fellowship of the Ring",
        StarsIn "Liv Tyler" "The Fellowship of the Ring"]

db :: MovieDB
db = (movies, stars, starsIn)


--Task2..
data NTree a = Nil | Node a [NTree a]
 deriving(Show, Eq)

t1 :: (Num a) => NTree a
t1 = Node 8 [Node 7 [Node 4 [], Node 5 []], Node 6 [Node 10 [], Node 15 [], Node 13 []], Node 18 []]

t2 :: NTree Char
t2 = Node '1' [Node 'i' [Node 'k' [Node 'L' [], Node 'I' [], Node 'e' []]], Node 'm' [Node 's' []], Node 'f' [Node 'a' [], Node 'H' []]]

degr ::(Eq a) =>NTree a -> a -> Int
degr Nil _ = 0
degr tree@(Node val succ) n = helper tree n 0
 where
     helper :: (Eq a) =>NTree a -> a -> Int -> Int
     helper (Node val succ) n depth
      | depth == 0 && val == n = length succ
      | val == n = 1 + length succ
      | otherwise = foldr (\x res -> res + (helper x n (depth + 1)) )  0 succ