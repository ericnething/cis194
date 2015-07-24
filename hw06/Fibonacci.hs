module Fibonacci where

-- | Calculate the nth fibonacci number
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | Produce the first n fibonacci numbers
fibs1 :: Integer -> [Integer]
fibs1 n = take (fromIntegral n) $ foldr (\x acc -> fib x : acc) [] [0..]

