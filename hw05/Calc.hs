{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser (parseExp)
import qualified StackVM as VM
import           StackVM (Program)
import Data.Monoid ((<>))

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr str =
  case parseExp Lit Add Mul str of
   Nothing -> Nothing
   Just e -> Just (eval e)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit n = n
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit n | n > 0     = True
        | otherwise = False
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 (a + b `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (a * b `mod` 7)

instance Expr Program where
  lit n   = return (VM.PushI n)
  add a b = a <> b <> return VM.Add
  mul a b = a <> b <> return VM.Mul

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

compile :: String -> Maybe Program
compile = parseExp lit add mul
