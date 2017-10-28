inSphere :: (Ord a, Num a) => [a] -> a -> Bool
inSphere xs r = (sum $ map (\x -> x * x) xs) <= r * r