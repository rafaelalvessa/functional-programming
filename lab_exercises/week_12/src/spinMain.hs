module Main  where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)
import Control.Exception (evaluate)

import Spin

main = do
  args <- getArgs
  let prog = case (read (head args)) of
    0 -> spins
    1 -> spinp
    2 -> spinp2
    3 -> spinp3
  let n = read (args !! 1)

  putStrLn $ "We have case " ++ head(args) ++ " and size " ++ (args !! 1)
  -- start <- getCurrentTime
  -- let go = evaluate $ prog n
  evaluate $ prog n
  -- putStrLn $ "Done. Result is " ++ (show go)
  -- end <- getCurrentTime
  -- putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
