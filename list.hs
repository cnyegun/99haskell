myLast :: [a] -> Maybe a
myLast xs = case xs of
    [y] -> Just y
    [] -> Nothing
    (_:ys) -> myLast ys

myButLast :: [a] -> Maybe a
myButLast xs = case xs of
    [y, _] -> Just y
    [] -> Nothing
    (_:ys) -> myButLast ys

elementAt :: [a] -> Int -> Maybe a
elementAt xs n 
    | n < 1 = Nothing
    | otherwise = case xs of 
        [] -> Nothing
        (y:ys) | n == 1 -> Just y
        (_:ys) -> elementAt ys (n - 1)

myLength :: [a] -> Int
myLength xs = helper xs 0 where
    helper ys rsf = case ys of
        [] -> rsf
        (_:rest) -> helper rest (rsf + 1)

myReverse :: [a] -> [a]
myReverse lst = helper lst [] where
    helper xs acc = case xs of 
        [] -> acc
        (first:rest) -> helper rest (first:acc)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome lst = myReverse lst == lst

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten xs = helper xs [] where
    helper :: NestedList a -> [a] -> [a]
    helper node acc = case node of
        Elem x -> x : acc
        List items -> foldr helper acc items

compress :: Eq a => [a] -> [a]
compress xs = case xs of
    (x:y:ys) | x == y -> compress (y:ys)
             | x /= y -> x : compress (y:ys)
    [x] -> [x]
    [] -> []

pack :: Eq a => [a] -> [[a]]
pack = undefined