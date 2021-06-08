module BasicConceptsExercises where

import BasicConcepts (factors)

-- | Returns a coordinate grid of m x n.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- | Returns a coordinate square of size n, excluding the diagonal.
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

myreplicate :: Int -> a -> [a]
myreplicate n x = [x | _ <- [1 .. n]]

-- | Returns all the pairs that satisfy the condition x^2 + y^2 = z^2 from 1..n
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], (x ^ 2 + y ^ 2) == z ^ 2]

-- | Returns all perfect numbers up to n
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (factors x) - x == x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]