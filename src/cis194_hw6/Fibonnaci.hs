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

{-
wholes :: Stream Integer
wholes = streamFromSeed (+ 1) 1

ruler' :: Integer -> Integer
ruler' n
    | odd n = 0
    | otherwise = 1 + ruler' (n `div` 2)

ruler :: Stream Integer
ruler = ruler' <$> wholes
-}

{-
n    a(n)
----------
1      0
2      1 = 1 + a(1) = 1 + 0
3      0
4      2 = 1 + a(2) = 1 + 1
5      0
6      1 = 1 + a(3) = 1 + 0
7      0
8      3 = 1 + a(4) = 1 + 2
9      0
10     1
11     0
12     2 = 1 + a(6)
13     0
14
15     0
16     0
-}

-- TODO: Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))
