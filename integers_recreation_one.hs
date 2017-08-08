listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n
  | m > n = []
  | squared_divisors_is_square $ squared_divisors m = [(m, squared_divisors m)] ++ (listSquared (m+1) n)
  | otherwise = [] ++ (listSquared (m+1) n)


squared_divisors :: Int -> Int
squared_divisors n = sumDivisorsN
  where sumDivisorsN = sum $ map (\x -> x*x) $ divs n


squared_divisors_is_square :: Int -> Bool
squared_divisors_is_square sumDivisorsN =  squaredN * squaredN == sumDivisorsN
  where squaredN = floor $ sqrt (fromIntegral sumDivisorsN)

divisors :: Int -> [Int]
divisors n =  [x | x <- [1..(n)], n `rem` x == 0]


divs n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)] ++ [n]