{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Tree
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e gl@(GL l f) = GL (e : l) (f + empFun e)

instance Semigroup GuestList where
    gl1@(GL l1 f1) <> gl2@(GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 > f2 then gl1 else gl2

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x children) = f x $ map (treeFold f) children

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subtree_gls = (glCons boss gl_wo_st_boss, gl_w_st_boss)
  where
    gl_wo_st_boss = mconcat $ map snd subtree_gls
    gl_w_st_boss = mconcat $ map (uncurry moreFun) subtree_gls

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
main :: IO ()
main = do
    company <- readFile "company.txt"
    let best_gl@(GL list fun) = maxFun (read company :: Tree Employee)
    putStrLn $ "Total fun: " ++ show fun
    mapM_ putStrLn $ sort $ map empName list
