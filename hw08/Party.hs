{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid

-- | Add an Employee to the GuestList and update the Fun score without
-- performing any checks
glCons :: Employee -> GuestList -> GuestList
glCons x@(Emp _ t0) (GL xs t) = GL (x:xs) (t + t0)

instance Monoid GuestList where
  mempty               = GL [] 0
  GL xs t1 `mappend` GL ys t2 = GL (xs <> ys) (t1 + t2)

-- | Given two GuestLists, produce the one with the higher Fun score
moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ t1) y@(GL _ t2)
  | t1 >= t2  = x
  | otherwise = y

