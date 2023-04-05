{-# LANGUAGE TypeSynonymInstances #-}

import           ExprT
import           Parser  (parseExp)

import qualified StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y
eval (Lit x)   = x


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
{-
evalStr s =
 case parseExp Lit Add Mul s of
   Nothing-> Nothing
   Just x -> Just $ eval x
-}


-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul


-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ min x y
    mul (MinMax x) (MinMax y) = MinMax $ max x y

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 . (`mod` 7) $ x + y
    mul (Mod7 x) (Mod7 y) = Mod7 . (`mod` 7) $ x * y


-- Exercise 5
instance Expr StackVM.Program where
    lit = (:[]) . StackVM.PushI
    add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
    mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul
