{-# LANGUAGE ViewPatterns #-}

-- Exercise 3
module Scrabble where

import Data.Char (toUpper)

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
    (Score x) <> (Score y) = Score $ x + y

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
score (toUpper -> c)
    | c `elem` "AEILNORSTU" = Score 1
    | c `elem` "DG" = Score 2
    | c `elem` "BCMP" = Score 3
    | c `elem` "FHVWY" = Score 4
    | c `elem` "K" = Score 5
    | c `elem` "JX" = Score 8
    | c `elem` "QZ" = Score 10
    | otherwise = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score
