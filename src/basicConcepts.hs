module BasicConcepts where

import Data.Char (chr, isLower, ord)

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
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- | Returns True if n is prime, False otherwise
prime :: Int -> Bool
prime n = factors n == [1, n]

-- | Returns the first nth prime numbers
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

-- zip returns pairs zip [a b c d] [d c b a] -> [(a, d), (b, c), (c, b), (d, a)]

-- | pairs [1, 2, 3, 4] == [(1,2), (2, 3), (3, 4)]
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- | Checks if a list is ordered
sorted :: Ord a => [a] -> Bool -- Ord a => restricts a to be an instance of Ord
sorted xs = and [x <= y | (x, y) <- pairs xs] -- x <= y for (x, y) in pairs xs

-- | Returns all the positions of x in xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x'] -- [0..] is an infinite list

-- 5.4

-- String = [Char] => String is a list of Char

-- | Counts the amount of lowercase letters
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- | Counts the occurences of c in cs
count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c' == c]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

-- | Encode Caesar cipher
-- Can also decode by using a negative number
encode :: Int -> String -> String
encode n xs = [shift n c | c <- xs]

-- | Calculates the percentage of one integer with respect to another
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- | Returns a frequency table for any String
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- | Crack the Caesar Cipher -> Doesn't work well with short strings or if it has an unusual distribution of letters.
crack :: String -> String
crack xs = encode (- factor) xs
  where
    table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1] :: [Float]
    table' = freqs xs
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    factor = head (positions (minimum chitab) chitab)
