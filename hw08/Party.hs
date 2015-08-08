{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Control.Applicative ((<$>))
import Data.Tree
import Data.List (foldl1', sort)

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

treeFold' :: (a -> [b] -> b) -> Tree a -> b
treeFold' f (Node a xs) = f a (map (treeFold' f) xs)

-- | Given a Boss and a list of pairs of GuestList with their boss,
-- and GuesList without their boss, calculate a pair of the best
-- GuestList with this Boss, and without this Boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss xs = (withBoss xs, withoutBoss xs)
  where withBoss = glCons boss . mconcat . snd . unzip
        withoutBoss = mconcat . map (uncurry moreFun)

-- | Calculate the best GuestList given an Employee hierarchy
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold' nextLevel


main :: IO ()
main = do
  (GL employees score) <- maxFun . read <$> readFile "company.txt"
  let guests = sort $ map empName employees
  putStrLn $ "Total fun: " <> show score
  mapM_ putStrLn guests
