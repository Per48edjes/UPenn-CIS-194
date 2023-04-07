main :: IO ()
main = undefined


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)


toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])


doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight = reverse . doubleEveryOther . reverse


sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum xs | xs <- map toDigits xs]


validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOtherFromRight . toDigits


type Peg = String
type Move = (Peg,Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n beg end temp
  | n <= 0 =  []
  | n == 1 =  [(beg,end)]
  | otherwise = 
      hanoi (n-1) beg temp end 
      ++ hanoi 1 beg end temp 
      ++ hanoi (n-1) temp end beg

sumConsecutive :: Integer -> Integer
sumConsecutive n = n * (n + 1) `div` 2

-- Props to Ezzeri Esa for refactoring my earlier (correct, but unscalable) solution into this!
hanoiFour' :: Integer -> Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour' m n beg end temp1 temp2
  | m == 2 && n <= 2 = hanoi n beg end temp1
  | n <= 0 = []
  | n <= (sumConsecutive m - 1) = hanoiFour' (m - 1) n beg end temp1 temp2
  | otherwise =
      hanoiFour' m (n-m) beg temp2 end temp1
      ++ hanoi m beg end temp1
      ++ hanoiFour' m (n-m) temp2 end beg temp1
