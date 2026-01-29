import Control.Monad (when)
data Item a = Single a | Multiple Int a deriving (Show, Eq)
-- | 
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeModified :: Eq a => [a] -> [Item a]
encodeModified xs = helper (pack xs)
    where 
    helper :: Eq a => [[a]] -> [Item a]
    helper ys = case ys of 
        [] -> []
        (car:cdr) -> if length car == 1 then Single (head car) : helper cdr
                        else Multiple (length car) (head car) : helper cdr
    pack :: Eq a => [a] -> [[a]]
    pack [] = []
    pack (x:xs) = (x:same) : pack rest
        where (same,rest) = span (== x) xs

foldr' f acc xs = case xs of 
    [] -> acc
    (y:ys) -> f y (foldr' f acc ys)

foldl' f acc xs = case xs of
    [] -> acc
    (y:ys) -> foldl' f (f y acc) ys

-- | Decode 
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [Item a] -> [a]
decodeModified = foldr' go []
    where 
        go (Single val) acc = val : acc
        go (Multiple count val) acc = replicate count val ++ acc




-- |
-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

encodeDirect :: Eq a => [a] -> [Item a]

-- encodeDirect [] = []
-- encodeDirect (x:xs) = 
--     if  count == 1 then Single x : encodeDirect rest
--                    else Multiple count x : encodeDirect rest
--     where 
--         goCount :: Eq a => a -> [a] -> Int -> (Int, [a])
--         goCount y ys acc = case ys of
--             [] -> (acc + 1, [])
--             (first:rest) | first == y -> goCount y rest (acc + 1)
--                          | otherwise -> (acc + 1, ys)
--         (count, rest) = goCount x xs 0

encodeDirect = foldr go []
    where
        go x [] = [Single x]
        go x (Single y : ys)
            | x == y = Multiple 2 x : ys
            | otherwise = Single x : Single y : ys
        go x (Multiple n y : ys)
            | x == y = Multiple (n + 1) y : ys
            | otherwise = Single x : Multiple n y : ys