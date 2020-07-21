import Data.Char

main = do
  process "file1.txt"
  process "file2.txt"
  process "file3.txt"
  process "file4.txt"
  print "Main thread done!"

process file = do
  stream <- readFile file
  let result = map toUpper stream
  writeFile (file ++ ".out") result
  print $ file ++ " processed"
