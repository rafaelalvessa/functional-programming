-- Haskell programs contain equations declaring identifiers.
x = 4

-- The order in which different identifiers are declared does not matter.
-- We can declare y depending on z.
y = z
-- And only later declare:
z = 3

-- But we must declare the z somewhere (uncomment this and reload).
-- Should get error: Not in scope: `v'.
-- u = v

-- And you can't declare stuff twice (uncomment this and reload).
-- z = 4

-- Simple form of declaration:
-- <FunctionName> <pattern(s)> = <expression>

foo x = x + 4

lplus x (y:ys) = x + y

threeplus x y z = x + y + z

-- But the clauses for each declaration have to be together.
-- lplus x [] = x

-- And you can't use the same variable more than once in the pattern part of a
-- declaration.
-- fourplus x x y z = 0

-- But you can in different clauses.
nlplus [] = 0
nlplus [x] = x
nlplus (x:y:ys) = x + y

-- Exercise 1

-- Haskell uses the off-side rule; definitions can go over more than one line:
goo x = if x <= 0 then 0 else x * x

-- Note that you need the else clause. Try commenting it out, or moving it to
-- the left.

-- HASKELL TYPES

-- We can also make type declarations.
type MyString = [Char]
-- Declares MyString as a synonym for [Char] (String already is...).

-- More interestingly:
data Name = Name String
            deriving Show
-- The last line is just so we can print stuff to screen; try commenting it out.

-- Or:
data MyBtree = Node [Char] MyBtree MyBtree | Leaf [Char] | Empty
               deriving Show

btreeExample = Node "a"
    (Node "b" (Leaf "c")  Empty)
    (Node "d" (Node "e" (Leaf "f") (Leaf "g")) Empty)

nameLength (Name s) = length s

top (Node s _ _) = s
top (Leaf s) = s
top Empty = ""

list (Node s l r) = s ++ (list l) ++ (list r)
list (Leaf s) = s
list (Empty) = ""

data IntChar = IntCharPair Int Char
               deriving Show

data IntList = IntNil | IntCons Int IntList
               deriving Show


data Pair a b = MkPair a b

data MyList a = Nil | Cons a (MyList a)

myLength Nil = 0
myLength (Cons a as) = 1 + (myLength as)

myLength' x = case x of { Nil -> 0 ; Cons a as -> 1+myLength' (as) }

myLength'' = \ x -> case x of { Nil -> 0 ; Cons a as -> 1 + myLength'' (as) }

tL = Cons 1 (Cons 2 Nil)

area x = let { pi = 3.14; xsq = x * x } in pi * xsq

fib x | x >= 1 = fib (x - 1) + fib (x - 2)
      | otherwise = 1


-- Exercise 2

filterNumber :: String -> String
filterNumber x = [c | c <- x, c /= ' ' && c /= '(' && c /= ')']

interNumber :: String -> String
interNumber x = "+44 " ++ (tail . filterNumber $ x)


-- Exercise 3

type Phone = [Char]

data Card = Card {
  name :: String,
  address :: String,
  phone :: Phone
} deriving (Show)

hasname :: String -> Card -> Bool
hasname x (Card a b c) = if x == a then True
                         else False

lookup' :: String -> Card -> Phone
lookup' x (Card a b c) = if (hasname x (Card a b c)) then c
                         else "Contact not found!"

alice = Card "Alice" "QMUL" "(01234) 567890"
