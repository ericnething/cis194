{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser (parseExp)
import qualified StackVM as VM
import           StackVM (Program)
import Data.Monoid ((<>))
import qualified Data.Map as M

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


class HasVars a where
  var :: String -> a

data VarExprT = XLit Integer
              | XAdd VarExprT VarExprT
              | XMul VarExprT VarExprT
              | XVar String
              deriving (Show, Eq)

instance HasVars VarExprT where
-- var v = XVar v
-- eta reduced to below:
  var = XVar

instance Expr VarExprT where
  lit = XLit
  add = XAdd
  mul = XMul

-- | This looks like the Reader Monad
-- (->) (M.Map String Integer) (Maybe Integer)
-- Reader (M.Map String Integer) (Maybe Integer)
instance HasVars (M.Map String Integer -> Maybe Integer) where
-- var str = \e -> M.lookup str e
-- var str e = M.lookup str e
-- eta reduction yields what we have below:
  var str = M.lookup str

-- | This is also Reader: (->) r a
instance Expr (M.Map String Integer -> Maybe Integer) where  
-- ignore the environment and lift the String into a default context
-- lit :: Integer -> (r -> a)
  lit n   = \_ -> return n

-- Both `a` and `b` are functions provided by `var` from the HasVars
-- typeclass that must be applied over an environment to obtain the
-- result
-- add :: a          -> a          -> a
--     :: ((->) r a) -> ((->) r a) -> ((->) r a)
--     :: (r -> a)   -> (r -> a)   -> (r -> a)
  add a b = \e -> case (a e, b e) of
                   (Just a', Just b') -> Just (a' + b')
                   _                  -> Nothing
-- mul :: (r -> a) -> (r -> a) -> (r -> a)
  mul a b = \e -> case (a e, b e) of
                   (Just a', Just b') -> Just (a' * b')
                   _                  -> Nothing

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
