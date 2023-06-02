module Eratosthenes where

{-
 This is an implemention of the Sieve of Eratosthenes for finding prime numbers.
 -}

main = do
    putStrLn "Enter the upper bound: "
    upper <- getLine
    let list = [2 .. (read upper)]
        primes = sieve [] list
    putStrLn ("Primes: " ++ show primes)

sieve :: [Int] -> [Int] -> [Int]
sieve primes list
    | null list = primes
    | otherwise = sieve (primes ++ [head list]) (filter (\x -> x `mod` head list /= 0) list)

-- Practice using folds
-- sieve :: [Int] -> [Int] -> [Int]
-- sieve primes list = foldr step primes list
--   where
--     step :: Int -> [Int] -> [Int]
--     step x acc = x : filter (\y -> y `mod` x /= 0) acc
