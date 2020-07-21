import Control.Monad
import Data.Char
import System.IO
import System.Environment

{-
  main = putStrLn "Hello, world!"
-}

{-
  main = do
    putStrLn "Greeting! What is your name?"
    inpStr <- getLine
    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
-}

{-
  main = do
    putStrLn "What is your name?"
    name <- getLine
    let bigName = map toUpper name
    putStrLn ("Hey " ++ bigName ++ ", how are you?!")
-}

reverseWords = unwords . map reverse . words

{-
  main = do
    line <- getLine
    if null line
      then return ()
    else do
      putStrLn (reverseWords line)
      main
-}

{-
  main = do
    line <- getLine
    when (not $ null line) $ do
      putStrLn $ reverseWords line
      main
-}

{-
  main = forever $ do
    line <- getLine
    when (not $ null line) (putStrLn $ reverseWords line)
-}

{-
  main = interact toUpperStr

  toUpperStr = map toUpper
-}

{-
  main = do
    print $ "Do you want me to say [G]oodbye, [H]ello, or just [Q]uit?"
    line <- getLine
    when (line == "G") $ do
      print "Goodbye!"
      main
    when (line == "H") $ do
      print "Hello!"
      main
-}

{-
  main = do
    args <- getArgs
    progName <- getProgName
    putStrLn $ (show.length $ args) ++ " arguments input"
    putStrLn $ "The program name is: " ++ progName
-}

{-
  main = do
    l <- getContents
    putStrLn $ map toUpper l
-}

{-
  main = do
    handle <- openFile "poem.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
-}

{-
  main = do
    contents <- readFile "poem.txt"
    writeFile "poem-cap.txt" (map toUpper contents)
-}

main = interact (show.length)
