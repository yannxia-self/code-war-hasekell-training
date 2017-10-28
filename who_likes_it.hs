likes :: [String] -> String
likes [] = "no one likes this"
likes (x:[]) = x ++ " likes this"
likes (x:y:[]) = x ++ " and " ++ y ++  " like this"
likes (x:y:xs) 
    | length xs > 1 = x ++ ", " ++ y ++ " and " ++ (show $ length xs) ++  " others like this"
    | otherwise = x ++ ", " ++ y ++ " and " ++ xs!!0 ++  " like this"