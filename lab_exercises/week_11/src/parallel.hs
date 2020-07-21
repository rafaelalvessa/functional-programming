import Control.Concurrent
import Data.Char

{-- Parallel code --}

main = do m1 <- newEmptyMVar :: IO (MVar Bool)
          m2 <- newEmptyMVar :: IO (MVar Bool)
          m3 <- newEmptyMVar :: IO (MVar Bool)
          m4 <- newEmptyMVar :: IO (MVar Bool)
          forkIO (process "file1.txt" m1)
          forkIO (process "file2.txt" m2)
          forkIO (process "file3.txt" m3)
          forkIO (process "file4.txt" m4)
          v1 <- takeMVar m1
          v2 <- takeMVar m2
          v3 <- takeMVar m3
          v4 <- takeMVar m4
          print "Main thread done!"

process file mvar = do
  stream <- readFile file
  let result = map toUpper stream
  writeFile (file ++ ".out") result
  print $ file ++ " processed"
  putMVar mvar True
