combinations :: Int -> [a] -> [[a]]
combinations
  where
    binLists = newBinList (2^n) n


binListToArray :: [Int] -> [Int]



newBinList :: Int -> Int -> [[Int]]
newBinList n s
  | n == 0 = [leftPadBin s [0]]
  | otherwise = [leftPadBin s $ decToBin n] ++ newBinList (n-1) s


decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

leftPadBin :: Int -> [Int] -> [Int]
leftPadBin n xs = if length xs >= n then xs else leftPadBin n $ leftPad xs
  where
    leftPad xs = [0] ++ xs