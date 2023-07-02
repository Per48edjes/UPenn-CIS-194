{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

import Control.Applicative (liftA2)
import Data.Char (digitToInt, isDigit)
import Data.List (foldl', transpose)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse

safeInit :: [a] -> Maybe [a]
safeInit = fmap reverse . safeTail . reverse

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs =
    filter (not . null) $
        takeWhile p xs : splitWith p (dropWhile (not . p) (dropWhile p xs))

{- See FirstWords.hs -}

transposeText :: String -> String
transposeText = unlines . transpose . lines

-- Example: Implement foldl as a foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
  where
    step x g a = g (f a x)

{-
e.g.,
  f = (+)
  xs = [1, 2, 3]
  z = 0

Expected behavior (how foldl is supposed to operate):

== myFoldl (+) 0             [1, 2, 3]
== myFoldl (+) (0+1)         [2, 3]
== myFoldl (+) ((0+1)+2)     [3]
== myFoldl (+) (((0+1)+2)+3) []
== (((0+1)+2)+3)

myFoldl behavior:

== (foldr step id [1,2,3]) 0
== (step 1 (foldr step id [2,3])) 0
== (step 1 (step 2 (foldr step id [3]))) 0
== (step 1 (step 2 (step 3 (foldr step id [])))) 0
== step 1 (step 2 (step 3 (id))) 0
== step 2 (step 3 (id)) (0+1)
== step 3 id ((0+1)+2)
== id (((0+1)+2)+3)
== (((0+1)+2)+3)
-}

asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold s@(c : cs)
    | c == '-' = (-1) * foldl' digitizer 0 cs
    | otherwise = foldl' digitizer 0 s
  where
    digitizer acc x
        | isDigit x = acc * 10 + digitToInt x
        | otherwise = error "Non-digit character in string"

asInt_either :: String -> Either String Int
asInt_either [] = Right 0
asInt_either s@(c : cs)
    | c == '-' = (* (-1)) <$> foldl' digitizer (Right 0) cs
    | otherwise = foldl' digitizer (Right 0) s
  where
    digitizer :: Either String Int -> Char -> Either String Int
    digitizer acc x
        | isDigit x = liftA2 (+) ((* 10) <$> acc) (pure (digitToInt x))
        | otherwise = Left $ concat ["non-digit ", "'", [x], "'"]

concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p xs = foldr f [] xs
  where
    f x acc
        | p x = x : acc
        | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' g xs = foldr f [] xs
  where
    f x [] = [[x]]
    f x (y : ys)
        | g x (head y) = (x : y) : ys
        | otherwise = [x] : y : ys

any' :: (a -> Bool) -> [a] -> Bool
-- foldr more efficient than foldl for short-circuiting
any' p xs = foldr f False xs
  where
    f x acc = p x || acc

cycle' :: [a] -> [a]
-- foldr more efficient than foldl for infinite lists
cycle' xs = foldr f [] [1 ..]
  where
    f _ acc = xs ++ acc

words' :: String -> [String]
-- foldr a more natural choice than foldl' because spaces are delimiters
-- at the end of words, not the beginning
words' s = foldr f [] s
  where
    f ' ' [] = [[]]
    f x [] = [[x]]
    f x (y : ys)
        | x == ' ' = [] : y : ys
        | otherwise = (x : y) : ys

unlines' :: [String] -> String
-- foldr a more natural choice than foldl' because we want to add a newline
-- at the end of each string, not the beginning
unlines' xs = foldr f [] xs
  where
    f x [] = x
    f x acc = x ++ '\n' : acc
