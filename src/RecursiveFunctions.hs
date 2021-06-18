module RecursiveFunctions where

fac :: Integral a => a -> a
-- fac n = product [1..n]
fac n
  | n <= 0 = 1
  | otherwise = n * fac (n - 1)

-- | Performs an ordered insert
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- | Insertion sort
isort :: Ord a => [a] -> [a]
isort = foldr insert []

quadratic :: (Ord a, Floating a) => a -> a -> a -> Maybe (a, a)
quadratic a b c
  | discriminant >= 0 && twoa /= 0 = Just (x, x')
  | otherwise = Nothing
  where
    discriminant = b ^ 2 - 4 * a * c
    twoa = 2 * a
    x = ((- b) + sqrt discriminant) / twoa
    x' = ((- b) - sqrt discriminant) / twoa

