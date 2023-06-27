{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isSpace)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

prependA :: (Applicative f) => f a -> f [a] -> f [a]
prependA = liftA2 (:)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure empty

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = p `prependA` zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

concatA :: (Applicative f) => f [a] -> f [a] -> f [a]
concatA = liftA2 (++)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = oneOrMore (satisfy isAlpha) `concatA` zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
    deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
    = A Atom
    | Comb [SExpr]
    deriving (Show)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces
  where
    parseAtom = A <$> ((N <$> posInt) <|> (I <$> ident))
    parseComb = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')
