module ExWeek2 where
  import Test.QuickCheck


  -- 1
  -- maxi x y returns the maximum of x and y
  maxi :: Real a => a -> a -> a
  maxi a b | a >= b    = a
           | otherwise = b

  prop_maxi_returns_max :: Real a => a -> a -> Bool
  prop_maxi_returns_max a b | a >= b    = maxiRes == a
                            | otherwise = maxiRes == b
                            where maxiRes = maxi a b

  prop_maxi2 :: Real a => a -> a -> Bool
  prop_maxi2 p q = maxi p q >= p && maxi p q >= q && maxi p q `elem` [p,q]

  -- 2
  -- sumsq n returns 1*1 + 2*2 + ... + n*n
  sumsq :: Integer -> Integer
  sumsq 0 = 0
  sumsq n = sumsq (n - 1) + n * n

  prop_sumsq :: Integer -> Bool
  prop_sumsq n = let n' = abs n in
                sumsq n' == (n' * (n' + 1) * (2 * n' + 1) `div` 6)

  -- 3
  {-
  To move n discs from peg A to peg C:
1. move n−1 discs from A to empty B. This leaves disc n alone on non empty (A or B)
2. move disc n from non empty (A or B) to C
3. move n-1 discs from B to C (B will now be A, and A will be B). Recursive call
   back to 1
  -}
  hanoi :: Integer -> Integer
  hanoi 0 = 0
  hanoi n = 2 * hanoi (n - 1) + 1
  {-
  10 rings require 1023 moves

  The advanced case with k posts is solved with the Frame–Stewart algorithm.
  -}

  -- 4
  -- fib n computes the nth Fibonacci number
  fib :: Integer -> Integer
  fib 0 = 1
  fib 1 = 1
  fib n = fib (n - 1) + fib (n - 2)
  {-
  The complexity is exponential O(2^n)
  The time for n+1 is double the time of n
  -}

  -- 5
  --fibAux i (fib n) (fib (n+1)) == fib (n+i)
  fibAux :: Integer -> Integer -> Integer -> Integer
  fibAux 0 a b = a
  fibAux i a b = fibAux (i - 1) b (a + b)

  fib2 :: Integer -> Integer
  fib2 n = fibAux n 1 1

  prop_fib p = fib2 n == fib n
               where n = abs p

  -- 6
