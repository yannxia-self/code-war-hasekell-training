import Data.List

sortArray :: [Int] -> [Int]
sortArray xs = replaceOdd xs oddSortList
  where oddSortList = sort $ filter odd xs

replaceOdd :: [Int] -> [Int] -> [Int]
replaceOdd xs [] = xs
replaceOdd (x:xs) os
  | odd x = [head os] ++ (replaceOdd xs (tail os))
  | otherwise = [ x ] ++ (replaceOdd xs os)