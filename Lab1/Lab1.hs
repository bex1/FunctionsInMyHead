-----------------------------------------------------------------------------
-- Daniel Bäckström & Marcus Lagerstedt
-- Group 3
-----------------------------------------------------------------------------

module Lab1 where
import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

{-
Part 1

Answer: k + 1 steps
-}

{-
Part 2
-}

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product (replicate (fromInteger k) (fromInteger n))

{-
Part 3
-}

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k    = power2 (n*n) (k `div` 2)
           | otherwise = n * power2 n (k-1)

{-
Part 4
-}

-- A
{-
One shall only test for inputs forfilling the preconditions. There is no
contract if the preconditions does not hold. Hence we should not test that
an error is thrown for negative k. However there is no documentation here
specifing the preconditions, but we assume that you imply that k >= 0 is a
precondition.

We will base our test cases on input space partitioning with edge cases,
where k >= 0 and n are integers.

Hence the test cases are drawn from these input space partitionings:
(1) k == 0 && n < 0
(2) k == 0 && n == 0
(3) k == 0 && n > 0
(4) k > 0 && n < 0
(5) k > 0 && n == 0
(6) k > 0 && n > 0

To improve on the quality of these tests one can take edge cases of these
partitionings which should be tested along with some arbitrary case of the
partitioning in quetion.
An edge case for k == 0 && n < 0 is n == 1 for example.
The arbitrary cases will however not be dynamically randomized since we would
 be unable to know what such case caused the fail without QuickCheck. Hence
 they have to be randomly predetermined. (we assume that QuickCheck should
 not be utilized here)

So we get:
(1) k == 0 && n < 0   testcases k == 0 && n == -1 and k == 0 && n == -17231
(2) k == 0 && n == 0  testcases k == 0 && n == 0
(3) k == 0 && n > 0   testcases k == 0 && n == 1 and k == 0 && n == 873228
(4) k > 0 && n < 0    testcases k == 1 && n == -1 and k == 1 && n == -123112
                        and k == 28923 && n == -1
(5) k > 0 && n == 0   testcases k == 1 && n == 0 and k == 938292 && n == 0
(6) k > 0 && n > 0    testcases k == 1 && n == 1 and k == 1 && n == 237372
                        and k == 2723287 && n == 1

The assumption here is that if for example k == 0 and n is a random integer < 0
and the test passes, then for any other random integer n' < 0 && n' /= n, the
test will likely pass since they belong to the same partitioning.

The test cases are motivated by input space partitioning often used in testing.
-}

-- B
prop_powers n k = let k' = abs k in
                  power n k' == power1 n k' && power1 n k' == power2 n k'

-- C
test_power2 :: Bool
test_power2 = and [uncurry prop_powers c | c <- test_cases]
              where
                test_cases = [(-1,0),(-17231,0),                --(1)
                              (0,0),                            --(2)
                              (1,0),(873228,0),                 --(3)
                              (-1,1),(-12311,1),(-1,28923),     --(4)
                              (0,1),(0,938292),                 --(5)
                              (1,1),(237372,1),(1,2723287)]     --(6)

-- D
{-
prop_powers passes QuickCheck test.
-}
