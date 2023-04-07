module Golf where

import Data.List

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = map (`everyNthHelper` xs) [0..length xs - 1]
  where everyNthHelper n = map snd . filter ((== n) . fst) . zip (cycle [0..n])


-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = [x]
localMaxima xs = map getSecond $ filter maximumHelper $ triplesHelper xs
  where triplesHelper xs = zip3 (init $ init xs) (init $ tail xs) (tail $ tail xs)
        maximumHelper (a,b,c) = b > a && b > c
        getSecond (_,b,_) = b


-- Exercise 3
histogram :: [Integer] -> String
histogram xs =
  let freq = countFreq (xs ++ [0..9])
      suffix = replicate 10 '=' ++ "\n" ++ concatMap show [0..9] ++ "\n"
   in concatMap (`plotHist` freq) (reverse [1..maximum freq]) ++ suffix
  where countFreq = map ((+ (-1)) . genericLength) . group . sort
        plotHist n freq = map ((\p -> if p then '*' else ' ') . (>= n)) freq ++ "\n"
