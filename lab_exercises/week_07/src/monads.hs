f x = let n="w:t" in
  "<" ++ n ++ ">" ++ x ++ "</" ++ n ++ ">"

g x = let n="w:b" in
  "<" ++ n ++ ">" ++ x ++ "</" ++ n ++ ">"


infixl 4 -->
a --> f = f a

-- Pipeline
-- "Hello" --> f --> g

n = "Hello"

f' x = do
  putStr("Name of object: ")
  n <- getLine
  return ("<" ++ n ++ ">" ++ x ++ "</" ++ n ++ ">")

-- Try pipelining again:
-- "Hello" --> f' --> f'

-- Actually need:
-- "Hello" --> f' >>= f'

data CInt a = CI (Int, a)
instance Monad CInt where
  return a = CI (0, a)
  (CI (n, a)) >>= f = let (CI (m, b)) = f a in CI (n + m, b)

tag a = CI (1, a)

return' x = Just x

y >>== f = case y of
  Just x -> f x
  Nothing -> Nothing

unpack (Just x) = x

foo :: Maybe Int -> Maybe Int
foo x = do
  n <- x
  let m = n * n
  return (m + 2)

y :: Maybe Int
y = fail "test"

z:: IO Int
z = fail "test"

data Log a = Lg (a, String)
  deriving Show

instance Monad Log where
  return a = Lg (a,"")
  (Lg (a,s)) >>= f = let (Lg (v,l)) = f a in Lg (v,s++l)


isBigGang :: Int -> Bool
isBigGang x = x > 3

isBigGang' :: Int -> Log Bool
isBigGang' x = Lg (x > 3, "Compared gang size to 3. ")

sizeGang :: [String] -> Int
sizeGang = length

sizeGang' :: [String] -> Log Int
sizeGang' names = Lg (length names, "Got number in gang.")

logGang' :: [String] -> Log [String]
logGang' [] = Lg ([], "Gang has nobody.")
logGang' ns = Lg ([n], "Gang is " ++ concat ns)

gang = ["Alice", "Bob", "Chloe", "David"]

newtype Parser a = Parser (String -> [(a, String)])

parse (Parser p) = p

instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])
