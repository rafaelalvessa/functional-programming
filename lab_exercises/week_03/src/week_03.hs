-- Simple recursive function

f 0 = 1
f n = 5 * f (n - 1)

-- Factorial function

fact 0 = 1
fact n = n * fact (n - 1)

-- Factorial function (version 2)

facterr n | n < 0 = error "Illegal argument: negative number"
          | n == 0 = 1
          | otherwise = n * facterr (n - 1)

-- Non-terminating (divergent) function

diverge n = 5 * diverge (n + 1)

-- In-class exercise I

g 0 = 1
g n = (n - 1) * g (n - 1)

-- In-class exercise II

power 0 = False
power 1 = True
power n = if even(n) then power(halve n) else False

halve 0 = 0
halve 1 = 0
halve n = 1 + halve (n - 2)

-- Example of multiple recursion

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 1)

-- Recursion on lists

lproduct [] = 1
lproduct (n:ns) = n * lproduct ns

myLength [] = 0
myLength (n:ns) = 1 + myLength ns

myMax [] = error "Empty list"
myMax [n] = n
myMax (n:ns) = max n (myMax ns)

-- Recursive data types

data Tree = Leaf Int | Node String Tree Tree

tree1 = Leaf 4
tree2 = Leaf 7
tree3 = Node "Very" tree1 (Node "Nice" tree1 tree2)

addLeafs (Leaf n) = n
addLeafs (Node _ t1 t2) = (addLeafs t1) + (addLeafs t2)

-- Multiple argument recursion

myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith op (x:xs) (y:ys) = (op x y) : myZipWith op xs ys

myDrop 0 xs = xs
myDrop n [] = []
myDrop n (_:xs) = myDrop (n-1) xs

-- Exercise 1

myExp :: Int -> Int -> Int
myExp a b
  | a < 0 = error "invalid argument: a < 0"
  | b < 0 = error "invalid argument: b < 0"
  | b == 0 = 1
  | b == 1 = a
  | otherwise = a * (myExp a (b - 1))

-- Exercise 2

-- length [1, 2, 3] = 1 + length [2, 3] =
--                  = 1 + 1 + length [3] =
--                  = 1 + 1 + 1 + length [] =
--                  = 1 + 1 + 1 + 0 =
--                  = 3

-- drop 3 [1, 2, 3, 4, 5] = drop 2 [2, 3, 4, 5] =
--                        = drop 1 [3, 4, 5] =
--                        = drop 0 [4, 5] =
--                        = [4, 5]

-- Exercise 3

myAnd :: [Bool] -> Bool
myAnd [] = error "invalid argument"
myAnd [x] = x
myAnd (x:xs) = if x == False then x else x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat [[]] = []
myConcat [x] = x
myConcat (x:xs) = x ++ (myConcat xs)

myReplicate :: a -> Int -> [a]
myReplicate a n
  | n < 0 = error "invalid argument: n < 0"
  | n == 0 = []
  | otherwise = [a] ++ (myReplicate a (n - 1))

mySelect :: [a] -> Int -> a
mySelect (x:xs) n
  | n < 0 = error "invalid argument: n < 0"
  | n == 0 = x
  | otherwise = (mySelect xs (n - 1))

myElem :: Int -> [Int] -> Bool
myElem _ [] = False
myElem i [x] = i == x
myElem i (x:xs)
  | i == x = True
  | otherwise = myElem i xs

-- Exercise 4

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x < y = x:(merge xs (y:ys))
  | y < x = y:(merge (x:xs) ys)
  | otherwise = x:y:(merge xs ys)

-- Exercise 5

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort . fst . halve' $ xs) (msort . snd . halve' $ xs)

halve' :: [Int] -> ([Int], [Int])
halve' [] = ([], [])
halve' [x] = ([x], [])
halve' (x:y:xys) = (x:(fst . halve' $ xys), y:(snd . halve' $ xys))

-- Exercise 6

sum' :: [Int] -> Int
sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + sum xs
