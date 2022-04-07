import Data.List
main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"

repeater :: [Char] -> (Int -> [Char] -> [Char]) 
repeater string = (\ x y -> intercalate y $ replicate x [ x | x <- string ])
