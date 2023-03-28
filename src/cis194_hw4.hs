import           Data.List



-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (+ (-2)) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate collatz
  where collatz k = if even k then k `div` 2 else 3 * k + 1


-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr treeAssembler Leaf
  where treeAssembler a acc =
          let new = Node 0 Leaf a Leaf
          in case acc of
            Leaf -> new
            Node h Leaf v Leaf -> Node (h + 1) new v Leaf
            Node h Leaf v right@(Node {}) -> Node h new v right
            Node h left@(Node {}) v Leaf -> Node h left v new
            Node h left@(Node lh _ _ _) v right@(Node rh _ _ _) | lh < rh -> let st = treeAssembler a left in Node (case st of (Node lh' _ _ _) -> max rh lh' + 1) st v right
                                                                | lh >= rh -> let st = treeAssembler a right in Node (case st of (Node rh' _ _ _) -> max lh rh' + 1) left v st


-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> f a : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\k -> 2*k +1) . sieve
  where sieve n = ([1..n] \\) $ map (\(i,j) -> i + j + 2*i*j) $ filter (uncurry (<=)) [(i,j) | i <- [1..n], j <- [1..n]]
