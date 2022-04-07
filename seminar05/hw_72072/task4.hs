import Data.List

main :: IO()
main = do
    -- print $ (applyN (\x -> 2 * x) 5) 2 
    -- print $ (applyN (\x -> div x 10) 2) 100 == 1


    print $ (partiallyApply (\x y -> 10 * x + y) 5) 10 == 60
--  print $    ( \ x y -> 10*x + y) 5 10


applyN :: (Eq t, Num t) => (c -> c) -> t -> c -> c
applyN f 0 = id
applyN f 1 = f
applyN f x = f . applyN f (x - 1)

partiallyApply :: (a -> b -> c) -> a -> (b -> c)
partiallyApply f x = f x
