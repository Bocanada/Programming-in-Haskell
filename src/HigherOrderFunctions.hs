module HigherOrderFunctions where


-- Composition operator:
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- | Applies a function twice
-- |  > twice (*2) 3 -> 12
-- |  > twice reverse [1, 2, 3] -> [1, 2, 3]
twice :: (a -> a) -> a -> a
-- twice f x = f (f x) or more simply:
twice f = f . f

odd :: Integer -> Bool
-- odd n = not (even n)
-- or:
odd = not . even

sumsqreven :: Integral a => [a] -> a
-- sumsqreven ns = sum (map (^ 2) (filter even ns)), or:
sumsqreven = sum . map (^ 2) . filter even

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)
