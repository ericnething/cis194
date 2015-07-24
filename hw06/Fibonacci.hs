module Fibonacci where

-- | Calculate the nth fibonacci number
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | Produce the first n fibonacci numbers
fibs1 :: Integer -> [Integer]
fibs1 n = take (fromIntegral n) $ foldr (\x acc -> fib x : acc) [] [0..]

-- | Efficient fibonacci generator
fibs2 :: [Integer]
fibs2 = let go x1 x2 = x1 : go x2 (x1 + x2) in go 0 1

-- | Infinite stream
data Stream a = a :- (Stream a)

-- | Preview the Stream
instance Show a => Show (Stream a) where
  show = (++ "... (Truncated to first 40)") . show . take 40 . streamToList

-- | Stream to infinite list
streamToList :: Stream a -> [a]
streamToList (x :- xs) = x : streamToList xs

-- | Generate a Stream containing infinite copies of the same element
streamRepeat :: a -> Stream a
streamRepeat x = x :- streamRepeat x

-- | Map over a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :- xs) = f x :- streamMap f xs

-- | Generate a Stream from a seed value
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x :- streamFromSeed f (f x)

-- | Stream of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0
