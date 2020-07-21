module Spin where

import Control.Parallel

-- spins always returns 0 but takes exponential time to do the calculation.
-- spins is purely sequential.
spins :: Integer -> Integer
spins x | x == 0 = 0
        | x == 1 = 0
        | otherwise = a + b
  where a = spins (x - 1)
        b = spins (x - 2)

-- spinp spawns a parallel thread on each call.
spinp :: Integer -> Integer
spinp x | x == 0 = 0
        | x == 1 = 0
        | otherwise = a `par` ( b `pseq` a + b)
  where a = spinp (x - 1)
        b = spinp (x - 2)

-- spinp2 does its first call in parallel, but then evaluates sequentially.
spinp2 :: Integer -> Integer
spinp2 x | x == 0 = 0
         | x == 1 = 0
         | otherwise = a `par` ( b `pseq` a + b)
  where a = spins (x - 1)
        b = spins (x - 2)

-- spinp3' does d calls in parallel, and then evaluates sequentially.
spinp3' :: Integer -> Integer -> Integer
spinp3' d x | d == 0 = spins x
            | x == 0 = 0
            | x == 1 = 0
            | otherwise = a `par` ( b `pseq` a + b)
  where a = spinp3' (d - 1) (x - 1)
        b = spinp3' (d - 1) (x - 2)

spinp3 :: Integer -> Integer
spinp3 = spinp3' 3
