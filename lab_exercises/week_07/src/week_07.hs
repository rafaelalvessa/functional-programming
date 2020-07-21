import Data.Text (pack, unpack, splitOn)
import System.IO
import System.Directory

-- Exercise 1

main = do
  putStr "Enter a positive whole number: "
  input <- getLine
  let number = read input :: Int
  print number
  if (even number) then do
    putStr "Congratulations, your number is even. Half of it is "
    putStrLn $ (show(div number 2)) ++ "."
  else do
    putStr "Congratulation, your number is odd. Its square is "
    putStrLn $ (show(sqrt (read input :: Float))) ++ "."

-- Exercise 2

data Student = Student {
  name :: String,
  q1 :: Int,
  q2 :: Int
}

-- Exercise 3

readStudent :: String -> Student
readStudent line = (Student (unpack $ column !! 0) (read (unpack $ column !! 1))
    (read (unpack $ column !! 2))) where column = splitOn (pack ",") (pack line)

tot :: Student -> Int
tot student = (q1 student) + (q2 student)

instance Show Student where
  show (Student name q1 q2) = name ++ "\t" ++ (show q1) ++ "\t" ++
                              (show q2) ++ "\t" ++
                              (show $ tot (Student name q1 q2))

contentsWithTotals :: String -> String
contentsWithTotals contents = "Name,Q1,Q2,Total\r" ++
    (appendTotal . tail $ records)
  where records = splitOn (pack "\r") (pack contents)
        appendTotal [] = ""
        appendTotal [x] = (unpack x) ++ "," ++
            (show . tot . readStudent $ (unpack x))
        appendTotal (x:xs) = (appendTotal [x]) ++ "\r" ++ (appendTotal xs)

writeWithTotals :: String -> String -> IO ()
writeWithTotals input output = do
  inputFileExists <- doesFileExist input
  if inputFileExists then do
    content <- readFile input
    putStrLn content
    let outputContent = contentsWithTotals content
    writeFile output outputContent
  else
    putStrLn ("Error: file " ++ input ++ " does not exist!")

-- Exercise 4

superRead :: String -> IO ()
superRead input = do
  inputFileExists <- doesFileExist input
  if inputFileExists then do
    content <- readFile input
    let output = replaceCR . contentsWithTotals $ content
    putStrLn $ output ++ "\n"
    let students = tail . lines $ output
    let size = length students
    let matrix = [[q1 . readStudent $ s | s <- students],
                  [q2 . readStudent $ s | s <- students],
                  [tot . readStudent $ s | s <- students]]
    let avg = [(fromIntegral . sum $ matrix !! x) / (fromIntegral size) |
               x <- [0..2]] :: [Float]
    putStrLn $ (show $ avg !! 0) ++ "," ++ (show $ avg !! 1) ++ "," ++
        (show $ avg !! 2) ++ "\n"
    let squares = [[((fromIntegral m) - avg !! x)^2 | m <- matrix !! x] |
                   x <- [0..2]]
    let stdDev = [(sqrt . sum $ squares !! x) / (fromIntegral size) |
                  x <- [0..2]]
    putStrLn $ (show $ stdDev !! 0) ++ "," ++ (show $ stdDev !! 1) ++ "," ++
        (show $ stdDev !! 2)
  else
    putStrLn ("Error: file " ++ input ++ " does not exist!")
  where replaceCR input = [if x == '\r' then '\n' else x | x <- input]
