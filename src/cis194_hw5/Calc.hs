{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


import           ExprT
import           VarExprT
import           Parser  (parseExp)

import qualified StackVM
import qualified Data.Map as M
import System.Console.Terminfo (functionKey)

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Mul x y) = eval x * eval y
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Lit x)   = x


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul


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


-- Exercise 6
class HasVars a where
    var :: String -> a

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = VarExprT.Lit
    add = VarExprT.Add
    mul = VarExprT.Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n m = Just n
    add e1 e2 m = (+) <$> e1 m <*> e2 m
    mul e1 e2 m = (*) <$> e1 m <*> e2 m

-- Test function for Exercise 6
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
