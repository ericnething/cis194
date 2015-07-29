module JoinList where

import Sized
import Data.Monoid

-- | Abstraction of a List
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- | Test Case
testJoinList :: JoinList Size Char
testJoinList =
  Append (Size 4)
  (Append (Size 3)
   (Single (Size 1) 'y')
   (Append (Size 2)
    (Single (Size 1) 'e')
    (Single (Size 1) 'a')))
  (Single (Size 1) 'h')

-- | Convert a JoinList to a List
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ xs ys) = jlToList xs <> jlToList ys

-- | Safe list indexing
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- | Append two JoinLists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
xs    +++ Empty = xs
Empty +++ ys    = ys
xs    +++ ys    = Append (tag xs <> tag ys) xs ys

-- | Retrieve the monoidal tag
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- | Produce the value at the provided index for a JoinList
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty           = Nothing
indexJ n (Single _ a)
  | n == 0               = Just a                   -- found
  | otherwise            = Nothing                  -- not found
indexJ n (Append m xs ys)
  | n < 0 ||
    n > getSize (size m) = Nothing                  -- out of bounds
  | n < leftSize         = indexJ n xs              -- go left
  | otherwise            = indexJ (n - leftSize) ys -- go right

  where leftSize = getSize . size . tag $ xs

-- | Test indexJ
testIndexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Bool
testIndexJ n xs = indexJ n xs == jlToList xs !!? n

-- | Drop the specified number of elements from the front of the
-- JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n xs | n <= 0  = xs
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append m xs ys)
  | n < leftSize     = dropJ n xs +++ ys
  | otherwise        = dropJ (n - leftSize) ys
                       
  where leftSize = getSize . size . tag $ xs

-- | Test dropJ
testDropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Bool
testDropJ n xs = jlToList (dropJ n xs) == drop n (jlToList xs)

