module Calc where

import ExprT
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr str =
  case parseExp Lit Add Mul str of
   Nothing -> Nothing
   Just e -> Just (eval e)

