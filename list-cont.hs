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