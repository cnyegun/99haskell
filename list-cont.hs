import Control.Monad (when)
import Data.Bifunctor (first)
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

-- | Duplicate the elements of a list
-- >>> dupli []
-- []
-- >>> dupli [1]
-- [1,1]
-- >>> dupli [1,2,3]
-- [1,1,2,2,3,3]

dupli :: [a] -> [a]
-- dupli = foldr (\x acc -> x:x:acc) []
dupli = concatMap (replicate 2)

-- | Replicate the elements of a list a given number of times. 
-- >>> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli xs n = concatMap (rep n) xs
    where 
        rep :: Int -> a -> [a]
        rep 0 x = []
        rep n x = x : rep (n - 1) x

-- | Drop
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = go xs 1
    where 
        go :: [a] -> Int -> [a]
        go [] _ = []
        go (y:ys) i 
            | i `mod` n /= 0 = y : go ys (i + 1)
            | otherwise = go ys (i + 1)
                                        
-- | Split in two path given the length of the first part
-- >>> split "abcdefghik" 3
-- ("abc","defghik")
-- >>> split "abc" 6
-- ("abc","")
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n = (x:first, rest) 
    where (first, rest) = split xs (n - 1)

-- | Extract a slice from a list given two range (inclusive)
-- >>> slice "abcdefghik" 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a]

slice [] _ _ = []
slice _ _ 0 = []
slice (x:xs) start end | start == 1 = x : slice xs 1 (end - 1)
                       | otherwise = slice xs (start - 1) (end - 1)

-- | Rotate
-- >>> rotate "abcdefgh" 3
-- "defghabc"
-- >>> rotate "abcdefgh" (-2)
-- "ghabcdef"

rotate :: [x] -> Int -> [x]
rotate xs n | n == 0 = xs
            | n > 0 = drop n xs ++ take n xs 
            | otherwise = drop (len + n) xs ++ take (len + n) xs
                where len = length xs

-- | Remove the K'th element from a list
-- >>> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> [a] -> (a, [a])
-- removeAt n xs
--     | n < 1 || n > length xs = error "Index out of bounds"
--     |otherwise = go 1 xs
--     where 
--         go i (x:xs) 
--             | i == n = (x, xs)
--             | otherwise = let (item, leftovers) = go (i+1) xs
--                         in (item, x:leftovers)

removeAt n xs | n < 1 = error "Index out of bounds"
removeAt _ [] = error "Index out of bounds"
removeAt 1 (x:xs) = (x, xs)
removeAt k (x:xs) = 
    let (removed, rest) = removeAt (k - 1) xs
    in (removed, x:rest)