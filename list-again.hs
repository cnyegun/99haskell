import System.Random (randomRIO)
-- | Insert an element at a given position into a list. Solutions 
-- >>> insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt :: (Eq a, Num a) => t -> [t] -> a -> [t]
insertAt c xs 1 = c:xs
insertAt c (x:xs) k = x : insertAt c xs (k - 1)
insertAt c [] k
    | k == 1 = [c]
    | otherwise = error "Index out of bounds"

-- | Create a list containing all integers within a given range. 
-- >>> range 4 9
-- [4,5,6,7,8,9]

range :: (Eq t, Num t) => t -> t -> [t]
range j k | j == k = [k]
          | otherwise = j : range (j + 1) k


rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect _ 0 = return []
rndSelect xs k =  do
    idx <- randomRIO (0, length xs - 1)
    let selected = xs !! idx
    rest <- rndSelect xs (k - 1)
    return (selected:rest)