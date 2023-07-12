{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
    deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random = first DV . randomR (1, 6)
    randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

-- Exercise 2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    attacker_rolls <- replicateM allowed_attackers die
    defender_rolls <- replicateM allowed_defenders die
    let sortRolls = reverse . sort . fmap unDV
        battleResult a d = if a > d then (0, -1) else (-1, 0)
        battleResults = zipWith battleResult (sortRolls attacker_rolls) (sortRolls defender_rolls)
        netCasualties = foldr (\(a', d') (a, d) -> (a + a', d + d')) (attackers bf, defenders bf) battleResults
     in return $ uncurry Battlefield netCasualties
  where
    allowed_attackers = min 3 $ attackers bf - 1
    allowed_defenders = min 2 $ defenders bf

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
    bf' <- battle bf
    if attackers bf' < 2 || defenders bf' == 0
        then return bf'
        else invade bf'

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    results <- replicateM 1000 $ invade bf
    return $ fromIntegral (sum . map indicatorRandomVar $ results) / 1000
  where
    indicatorRandomVar :: Battlefield -> Integer
    indicatorRandomVar bf
        | defenders bf == 0 = 0
        | otherwise = 1

-- Exercise 5
-- Will need to review my probability fundamentals before trying to find an
-- analytical solution! ;)
