import Data.List (delete, minimum)


-- Practice: Implement merge sort
mergeHelper :: (Ord a) => [a] -> [a] -> [a]
mergeHelper [] right = right
mergeHelper left [] = left
mergeHelper left@(l:ls) right@(r:rs)
  | l <= r  = l : mergeHelper ls right
  | otherwise = r : mergeHelper left rs


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeHelper (mergeSort left) (mergeSort right)
  where (left, right) = splitAt ((`div` 2) $ length xs) xs


-- Practice: Implement insertion sort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort xs = foldl insertHelper [] xs
  where insertHelper acc x
          | null acc = [x]
          | otherwise = takeWhile (<= x) acc ++ [x] ++ dropWhile (<= x) acc


-- Practice: Implement selection sort
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let smallest = minimum xs in
                       smallest : selectionSort (delete smallest xs)


-- Practice: Implement quick sort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (pivot:xs) = quickSort (filter (< pivot) xs) ++ [pivot] ++ quickSort (filter (>= pivot) xs)
