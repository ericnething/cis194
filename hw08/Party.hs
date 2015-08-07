{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List (foldl1')

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

-- | Fold a Rose Tree
treeFold :: Monoid m => (a -> m) -> Tree a -> m
treeFold f (Node a xs) = f a <> mconcat (fmap (treeFold f) xs)

-- | Given a Boss and a list of pairs of GuestList with their boss,
-- and GuesList without their boss, calculate a pair of the best
-- GuestList with this Boss, and without this Boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss xs = (foldl1' moreFun withBoss, foldl1' moreFun withoutBoss)
  where withBoss = map (glCons boss . snd) xs
        withoutBoss = foldr' (\(a,b) acc -> glCons boss a : glCons boss b : acc) [] xs

-- | Calculate the best GuestList given an Employee hierarchy
-- maxFun :: Tree Employee -> GuestList
-- maxFun = treeFold ((:[]) . nextLevel)
