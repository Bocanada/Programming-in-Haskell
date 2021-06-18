module HigherOrderFunctionsExercises where

import Data.Char (chr, ord)
import Data.List (sort)
import HigherOrderFunctions (unfold)
-- Binary string transmitter
type Bit = Int

-- | bits should be in reverse order
-- TODO: Add . reverse so we don't have to write bits in reverse order
-- bin2Int bits = sum $ [b * w | (w, b) <- zip weights $ reverse bits]
--   where
-- weights = iterate (* 2) 1
bin2Int :: [Bit] -> Int
bin2Int = foldr (\x y -> x + 2 * y) 0

-- | The reverse function of bin2Int
int2Bin :: Int -> [Bit]
int2Bin = unfold (== 0) (`mod` 2) (`div` 2)

-- int2Bin 0 = []
-- int2Bin n = n `mod` 2 : int2Bin (n `div` 2)

-- | Converts a number into an 8-bit number
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- | Encodes a String into a list of bits
encode :: String -> [Bit]
encode = concatMap (make8 . int2Bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- | Decodes a list of bits into a String
decode :: [Bit] -> String
decode = map (chr . bin2Int) . chop8

-- | Simulates the transmission of a String as a list of bits using a perffect communication channel
transmit :: String -> String
transmit = decode . channel . encode

channel :: a -> a
channel = id

-- Voting algorithms

-- First past the post
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- | Removes duplicates from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

-- | The winner is the second component of the last result
winner :: Ord a => [a] -> a
winner = snd . last . result

-- -----------------

-- Alternative Vote
ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

-- | Removes all empty lists
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- | Eliminates x from a list of lists.
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map $ filter (/= x)

-- | Ranks the 1st-choice candidates in each ballot in increasing order
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] -> c
  (c : cs) -> winner' (elim c bs)

-- Exercises

-- all' p = foldr ((\x xs -> if p x then xs else xs) . p) []
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x : xs else []) []

-- takeWhile' _ [] = []
-- takeWhile' p (x : xs)
--   | p x = x : takeWhile' p xs
--   | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

-- myMap f = foldr f []
-- map' :: (a -> b) -> [a] -> [a]
map' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map' f = foldr (\x xs -> f x : xs) []

filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' f = foldr (\x xs -> if f x then x : xs else xs) []

-- | Converts a decimal number into an integer
-- | > dec2int [2,3,4,5]
-- | 2345
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y