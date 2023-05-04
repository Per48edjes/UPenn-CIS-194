{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x rest) = x : streamToList rest

instance (Show a) => Show (Stream a) where
  show = show . take 100 . streamToList

instance Functor Stream where
  fmap = streamMap

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x rest) = Cons (f x) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 rest1) s2 = Cons x1 (interleaveStreams s2 rest1)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) ((+ 1) <$> ruler)

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

one :: Stream Integer
one = Cons 1 (streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons x rest) = Cons (-x) (negate rest)
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) a@(Cons x xs) b@(Cons y ys) = Cons (x * y) (((* x) <$> ys) + (xs * b))

instance Fractional (Stream Integer) where
  (/) a@(Cons x xs) b@(Cons y ys) = Cons (x `div` y) ((one / y') * (xs - ((a / b) * ys)))
    where
      y' = Cons y (streamRepeat 0)

fibs3 :: Stream Integer
fibs3 = x / (one - x - (x ^ 2))
