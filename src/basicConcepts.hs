module BasicConcepts where

-- Sequencing
seqn :: Monad m => [m a] -> m [a]
seqn (act : acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)
seqn [] = return []


-- QickSort
qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x : xs) = qSort smallest ++ [x] ++ qSort biggest
  where
    smallest = [n | n <- xs, n <= x]
    biggest = [n | n <- xs, n > x]

-- Reverse sort
rSort :: Ord a => [a] -> [a]
rSort [] = []
rSort (x : xs) = rSort biggest ++ [x] ++ rSort smallest
  where
    biggest = [n | n <- xs, n > x]
    smallest = [i | i <- xs, i <= x]

luhnDouble :: Int -> Int
luhnDouble n = if result > 9 then result - 9 else result
  where
    result = n * 2

-- | Luhn Algorithm applied to 4 nums
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = result `rem` 10 == 0
  where
    result = sum [z, luhnDouble y, x, luhnDouble w]

-- | Returns the factors of n
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


-- | Returns True if n is prime, False otherwise
prime :: Int -> Bool
prime n = factors n == [1, n]  

-- | Returns the first nth prime numbers
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]