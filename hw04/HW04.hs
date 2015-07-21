module HW04 where

import qualified Data.Vector as V
import Data.List

-- | For testing fun1
fun1' :: [Integer] -> Integer
fun1' []       = 1
fun1' (x:xs) | even x    = (x - 2) * fun1' xs
             | otherwise = fun1' xs

-- | Point-free form of fun1'
--
-- >>> fun1' [3..20] == fun1 [3..20]
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- | For testing fun2
fun2' :: Integer -> Integer
fun2' 1             = 0
fun2' n | even n    = n + fun2' (n `div` 2)
        | otherwise = fun2' (3*n + 1)

-- | Point-free form of fun2'
--
-- >>> fun2' 20 == fun2 20
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate f
  where f n | even n    = n `div` 2
            | otherwise = 3*n + 1


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- | Generate a balanced binary tree from a list of values
foldTree :: [a] -> Tree a
foldTree xs = go (V.fromList xs)
  where go vec | V.null vec = Leaf
               | otherwise  = Node height (go left) (vec V.! mid) (go right)
          where mid    = V.length vec `div` 2
                left   = V.take mid vec
                right  = V.drop (mid + 1) vec
                height = floor . logBase 2 . fromIntegral . V.length $ vec

layoutTree :: Show a => Tree a -> [String]
layoutTree Leaf = []
layoutTree (Node h l a r) =
  indent (layoutTree l) ++ [show a ++ " (" ++ show h ++ ")"] ++ indent (layoutTree r)
  where indent = map ("  " ++)

printTree :: Show a => Tree a -> String
printTree = unlines . layoutTree


-- | Exclusive-OR over a list of Boolean values
xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

-- | map using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- | foldl using foldr
--
-- Original definition of foldl:
-- > foldl :: (a -> b -> a) -> a -> [b] -> a
-- > foldl f z [] = z
-- > foldl f z (x:xs) = foldl f (f z x) xs
--
-- Transform it:
-- > foldl f z xs = g xs z
-- >   where g [] z     = z
-- >         g (x:xs) z = g xs (f z x)
--
-- Move the last parameter to the other side using a lambda:
-- > foldl f z xs = g xs z
-- >  where go []     = \z -> z -- equivalent to id
-- >        go (x:xs) = \z -> g xs (f z x)
--
-- Now for the final substitution:
-- > foldl f z xs = g xs z
-- >   where go []     = id
-- >         go (x:xs) = \z -> g xs (f z x)
--
-- 

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a bs = foldr (\b g x -> g (f x b)) id bs a


-- | Sieve of Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : map (\x -> x*2 + 1) notPrimes
  where notPrimes = (\\) [1..n] . filter (<= n) $
                    [a + b + 2*a*b | a <- [1..n], b <- [1..a]]
