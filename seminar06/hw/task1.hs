main :: IO()
main = do
    print $ (rf ((-) (-4)) (* (-2)))  [1..10] (* 3) == [15,18,21,24,27,30] -- only 5, 6, 7, 8, 9 and 10 satisfy the condition        

rf :: (Ord a1) => (t -> a1) -> (t -> a1) -> [t] -> (t -> a2) -> [a2]
rf f g = (\xs h-> [h x | x <- xs, f x > g x])

-- Не знаех как да направя дефиницията затова просто написах t: rf в конзолата и то ми изведе това