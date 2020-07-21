-- DIFFERENCE BETWEEN DATA AND TYPE

-- Use type as an abbreviation for an existing type.

type Position = (Int, Int)
type Distance = Int

translate :: Position -> Distance -> Position
translate (x, y) d = (x + d, y)

-- phonebook

type PhoneNumber = String
type Name = String
type Entry = (Name, PhoneNumber)
type PhoneBook = [Entry]

phoneBook :: PhoneBook
phoneBook = [
  ("peter", "5555-4444"),
  ("paul", "1234-5678"),
  ("john", "9876-4321")
]

-- Find if person "name" is in given phone book.
inPhoneBook :: Name -> PhoneBook -> Bool
inPhoneBook name book = elem name (map fst book)

-- Use data to create a new data type.

data Move = North | South | East | West

move :: Position -> Move -> Position
move (x, y) West = (x - 1, y)
move (x, y) East = (x + 1, y)
move (x, y) South = (x, y - 1)
move (x, y) North = (x, y + 1)

-- Types and data can have other types as parameter.

type Table k v = [(k, v)]

findPersonAge :: String -> Table String Int -> Int
findPersonAge name [] = -1
findPersonAge name ((n,a):xs) = if (name == n) then a else findPersonAge name xs

delete :: Int -> Table Int String -> Table Int String
delete id [] = []
delete id ((n,a):xs) = if (id == n) then delete id xs else (n, a):(delete id xs)

-- Type declarations can be nested but not recursive.

type Board = [Position]

data Tree = Leaf Int | Node [Tree]

-- In-class poll.

type BoolFct = Bool -> Bool

type Function a = a -> a

type FunctionAB a b = a -> b

-- Type Nested a = [Nested a].

data Answer = NTD | Answer Bool

-- Data FunctionD a = a -> a.

data FunctionD a = Function (a -> a)

-- In-class exercise.

second xs = head (tail xs)

twice f x = f (f x)

palindrome xs = (reverse xs) == xs

double x = x * 2

-- TYPE CLASSES

-- Type class Eq.

remove :: Eq a => a -> [a] -> [a]
remove a [] = []
remove x (y:ys) = if x == y then ys else y : remove x ys

fstPair :: Eq a => (a, b) -> (a, c) -> Bool
fstPair (x, _) (y, _) = x == y

-- Type class Ord.

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x : y : ys
                | otherwise = y : (insert x ys)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
  smaller = [a | a <- xs, a < x]
  larger = [a | a <- xs, a >= x]

-- Derived instances.

data Person = Person {
  fName :: String,
  lName :: String,
  age :: Int
} deriving (Eq, Show)

mike = Person {fName = "Michael", lName = "Diamond", age = 43}
  
adam = Person {fName = "Adam", lName = "Horovitz", age = 41}

-- New data types deriving methods from type classes.
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Enum)

exp1 = Wed == Tue
exp2 = Wed < Tue
exp3 = show Wed
exp4 = read "Mon" :: Day
exp5 = [Mon .. Fri]

-- Creating a new type class.

type Side = Double
type Radious = Double

data Square = Square Side deriving (Show)
data Rectangle = Rectangle Side Side deriving (Show)
data Circle = Circle Radious deriving (Show)

class Shape a where
  perimeter :: a -> Double
  area :: a -> Double

instance Shape Square where
  perimeter (Square x) = 4 * x
  area (Square x) = x^2

instance Shape Rectangle where
  perimeter (Rectangle x y) = 2 * x + 2 * y
  area (Rectangle x y) = x * y

instance Shape Circle where
  perimeter (Circle x) = 2 * x * pi
  area (Circle x) = pi * x^2

areas :: Shape a => [a] -> [Double]
areas = map area

circles = [Circle 1.0, Circle 2.0, Circle 3.0]

-- THE MAIN FUNCTION

main :: IO ()
main = do
  print "Hi, what's your name?"
  name <- getLine
  print $ "Dear " ++ name ++ ", do you wanna be my friend?"

-- Exercise 1

lookUp :: Name -> PhoneBook -> PhoneNumber
lookUp _ [] = error "name not found"
lookUp n (pb:pbs)
  | n == fst pb = snd pb
  | otherwise = lookUp n pbs

lookUp' :: Name -> PhoneBook -> Maybe PhoneNumber
lookUp' _ [] = Nothing
lookUp' n (pb:pbs)
  | n == fst pb = Just (snd pb)
  | otherwise = lookUp' n pbs

-- Exercise 2

data Tree' = Leaf' Int | Node' Tree' Tree' deriving (Show)

binTree = Node'
    (Node'
        (Leaf' 0)
        (Node' (Leaf' 1) (Node' (Leaf' 2) (Leaf' 3)))
    )
    (Node' (Leaf' 4) (Leaf' 5))

balancedBinTree1 = Node' (Node' (Leaf' 0) (Leaf' 1))
                         (Node' (Leaf' 2) (Leaf' 3))

balancedBinTree2 = Node' (Node' (Leaf' 0) (Node' (Leaf' 1) (Leaf' 2)))
                         (Node' (Leaf' 3) (Leaf' 4))

balanced :: Tree' -> Bool
balanced (Leaf' _) = True
balanced (Node' lt rt) =
    let leftLeaves = numLeaves lt
        rightLeaves = numLeaves rt
    in ((leftLeaves >= rightLeaves - 1) && (leftLeaves <= rightLeaves + 1))

numLeaves :: Tree' -> Int
numLeaves (Leaf' _) = 1
numLeaves (Node' (Leaf' _) rt) = 1 + numLeaves rt
numLeaves (Node' lt (Leaf' _)) = 1 + numLeaves lt
numLeaves (Node' lt rt) = numLeaves lt + numLeaves rt

-- Exercise 3

class NumberLike a where
  add :: a -> a -> a
  sub :: a -> a -> a

instance NumberLike Bool where
  add x y
    | x == False && y == False = False
    | otherwise = True
  sub x y
    | x == True && y == False = True
    | x == False && y == False = True
    | otherwise = False

instance NumberLike [a] where
  add xs ys = xs ++ ys
  sub xs ys = xs
