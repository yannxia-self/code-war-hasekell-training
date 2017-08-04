import Data.List


maxSum :: [Int] -> [(Int,Int)] -> Int
maxSum xs xy = foldl' (\x y -> max x $ pairSum xs y) minValue xy
  where minValue = sum $ filter  (\x -> x < 0) xs

pairSum :: [Int] -> (Int, Int) -> Int
pairSum xs xy = sum $ spiteList xs xy

spiteList :: [Int] -> (Int, Int) -> [Int]
spiteList xs (x, y) = drop dropN $ take takeN xs
  where takeN = y + 1
        dropN = x