revR :: [Int] -> [Int]
revR [] = []
revR (x:[]) = [] ++ [x]
revR (x:xs) = revR(xs) ++ [x]