import Data.List


main :: IO ()
main = do
    print $ prime_factors 8612


prime_factors :: Integer -> String  
prime_factors n = prime_factors_string $ group $ prime_factors_nums n 2


prime_factors_string :: [[Integer]] -> String
prime_factors_string [] = ""
prime_factors_string (x:xs) 
    | length x == 1 = "(" ++ show (x !! 0) ++ ")" ++ (prime_factors_string xs)
    | otherwise = "(" ++ show (x !! 0) ++ "**" ++ show (length x) ++ ")" ++ (prime_factors_string xs)



prime_factors_nums :: Integer -> Integer -> [Integer]
prime_factors_nums n m 
    | n < m = []
    | n >= m && ((mod n m) == 0) = [m] ++ (prime_factors_nums (div n m) m)
    | n >= m = prime_factors_nums n (m + 1)
    | otherwise = []