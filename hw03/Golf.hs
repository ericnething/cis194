module Golf where

import qualified Data.Vector as V
import Data.List

-- | Equivalent to (init . tails)
skips :: [a] -> [[a]]
skips [] = []
skips (x:xs) = (x:xs) : skips xs

-- | Find the local maxima of the list of Integers
localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
  | x2 > x3 && x2 > x1 = x2 : localMaxima (x3:xs)
  | otherwise = localMaxima (x2:x3:xs)
localMaxima _ = []

-- | Generate a vertical histogram of the list
histogram :: [Integer] -> String
histogram = unlines . reverse . transpose . V.toList . pad . foldr update empty
  where update x acc = acc V.// [(fromIntegral x, acc V.! (fromIntegral x) ++ "*")]
        empty = V.generate 10 (\x -> show x ++ "=")
        pad vec = let maxLength = V.maximum (V.map length vec)
                      padding x = x ++ (replicate (maxLength - (length x)) ' ')
                  in V.map padding vec
