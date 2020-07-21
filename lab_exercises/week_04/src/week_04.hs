-- Exercise 1

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldl (\acc x -> (p x) || acc) False

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldl (\acc x -> (p x) && acc) True

mySum :: (Num a) => [a] -> a
mySum = foldl (+) 0

myProduct :: (Num a) => [a] -> a
myProduct = foldl (*) 1

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = foldr ((++) . f) []

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "empty list"
myMaximum (x:xs) = foldl (\acc x -> if acc > x then acc else x) x xs

myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "empty list"
myMinimum (x:xs) = foldl (\acc x -> if acc < x then acc else x) x xs

-- Exercise 2

-- mySelect and myElem are suitable to be implemented using fold. myReplicate
-- isn't because it doesn't take any list as an argument to be used with fold.

-- Exercise 3

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let splitLT = quicksort [a | a <- xs, a < x]
        splitGT = quicksort [a | a <- xs, a >= x]
    in  splitLT ++ [x] ++ splitGT
