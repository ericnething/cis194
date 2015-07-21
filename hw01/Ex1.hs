module Ex01 where

import Data.List

reverse' :: [a] -> [a]
reverse' xs = go xs []
  where go [] ys = ys
        go (x:xs) ys = go xs (x:ys)

-- | Convert a positive integer to a list of its digits
toDigits :: Integer -> [Integer]
toDigits n = go n []
  where go n xs
          | n > 0     = go (n `div` 10) (n `mod` 10 : xs)
          | otherwise = xs

-- | Reverse toDigits
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse' . toDigits

-- | Double every other element in the list starting on the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse' . zipWith ($) (cycle [id,(*2)]) . reverse'

-- | Sum all the digits in the list
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- | Check if the credit card number is valid
validate :: Integer -> Bool
validate = check . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
  where check 0 = True
        check _ = False


-- Towers of Hanoi
----------------------------------------------------------------------
type Peg = String
type Move = (Peg, Peg)

-- | Compute the list of moves necessary to move the stack of N discs
-- from peg A to peg B
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
