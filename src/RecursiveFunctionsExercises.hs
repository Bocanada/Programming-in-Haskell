module RecursiveFunctionsExercises where

-- | Sums from n to 0
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n -1)

-- | Euclid's algorithm
euclid :: Integral n => n -> n -> n
-- euclid 0 m = m
euclid n 0 = n
euclid n m
  | n <= m = euclid n (m - n)
  | otherwise = euclid (n - m) m

-- | Recursive and
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

-- | Recursive concat
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

-- | Recursive replicate
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n -1) x

-- | Recursive (!!)
index :: [a] -> Int -> a
index (x : xs) n
  | n == 0 = x
  | otherwise = index xs (n -1)

-- | Recursive elem
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs)
  | e == x = True
  | otherwise = myElem e xs

-- | Merges two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- | Splits a list in half
halve :: [a] -> ([a], [a])
halve xs = splitAt half xs where half = length xs `div` 2

-- | Recursive merge sort
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs

-- | Recursive sum
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- | Recursive take
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 xs = xs
myTake n (x : xs) = x : myTake (n -1) xs

-- | Recursive last
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs