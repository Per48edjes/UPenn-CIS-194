{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Buffer
import Editor
import Scrabble
import Sized

-- Exercise 1
data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

instance (Monoid m) => Semigroup (JoinList m a) where
    (<>) = (+++)

instance (Monoid m) => Monoid (JoinList m a) where
    mempty = Empty

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x) = if i == 0 then Just x else Nothing
indexJ i (Append s left right)
    | inRange && leftSize <= i = indexJ (i - leftSize) right
    | inRange && leftSize > i = indexJ i left
    | otherwise = Nothing
  where
    inRange = i >= 0 && (getSize . size $ s) > i
    leftSize = getSize . size . tag $ left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ i jl@(Single _ _) = if i <= 0 then jl else Empty
dropJ i (Append s left right)
    | leftSize <= i = dropJ (i - leftSize) right
    | otherwise = dropJ i left
  where
    leftSize = getSize . size . tag $ left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ i jl@(Single _ _) = if i <= 0 then Empty else jl
takeJ i (Append s left right)
    | leftSize <= i = left +++ takeJ (i - leftSize) right
    | otherwise = takeJ i left
  where
    leftSize = getSize . size . tag $ left

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ s) = s
    toString (Append _ left right) = toString left ++ toString right

    fromString = foldr ((+++) . (\l -> Single (scoreString l, Size 1) l)) Empty . lines

    line = indexJ

    replaceLine i s jl = case indexJ i jl of
        Nothing -> jl
        Just _ -> takeJ i jl +++ fromString s +++ dropJ (i + 1) jl

    numLines = getSize . snd . tag

    value = getScore . fst . tag

main = runEditor editor start
  where
    start =
        ( fromString $
            unlines
                [ "This buffer is for notes you don't want to save, and for"
                , "evaluation of steam valve coefficients."
                , "To load a different file, type the character L followed"
                , "by the name of the file."
                ]
        ) ::
            JoinList (Score, Size) String
