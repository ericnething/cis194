{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (replicateM)
import Control.Monad.Random
import Data.List (sortBy, foldl')

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield =
  Battlefield
  { attackers :: Army
  , defenders :: Army
  } deriving Show

-- | Roll n dice
dice :: Int -> Rand StdGen [DieValue]
dice n = sequence $ replicate n die

-- | Pair up the dice rolls for analysis
pair :: Ord a => [a] -> [a] -> [(a,a)]
pair xs ys = zip (desc xs) (desc ys)
  where desc = sortBy (flip compare)

-- | Can attack with up to 3 units, but must leave at least one behind
attackingUnits :: Army -> Army
attackingUnits n = min (n-1) 3

-- | Can defend with up to 2 units
defendingUnits :: Army -> Army
defendingUnits n = min n 2

outcome :: (DieValue, DieValue) -> (Army, Army) -> (Army, Army)
outcome (atk, def) (atkArmy, defArmy)
  | atk > def = (atkArmy, defArmy - 1)
  | otherwise = (atkArmy - 1, defArmy)

-- | Simulate a battle
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atk def) = do
  atkRolls <- dice (attackingUnits atk)
  defRolls <- dice (defendingUnits def)
  let result = foldr outcome (atk, def) $ pair atkRolls defRolls
  return $ uncurry Battlefield result

-- | Simulate a sequence of battles
invade :: Battlefield -> Rand StdGen Battlefield
invade game@(Battlefield atk def)
  | atk > 1 && def > 0 = invade =<< battle game
  | otherwise          = return game

-- | Estimate the probability of success for the attacking army
successProb :: Battlefield -> Rand StdGen Double
successProb game = return . probability =<< simulations
  where simulations = replicateM 1000 (invade game)
        probability = (/1000) . fromIntegral . fst . foldl' f (0,0)
        f (atkWins, defWins) (Battlefield atk def)
          | def > 0   = (atkWins, defWins + 1)
          | otherwise = (atkWins + 1, defWins)

run :: Show a => Rand StdGen a -> IO ()
run rand = putStrLn . show =<< evalRandIO rand
