import Data.List

main :: IO ()
main = print $ primeFactors 8612

primeFactors :: Integer -> String  
primeFactors n = primeFactorsString $ group $ primeFactorsNums n 2

primeFactorsString :: [[Integer]] -> String
primeFactorsString [] = ""
primeFactorsString (x:xs) 
    | length x == 1 = "(" ++ show (head x) ++ ")" ++ primeFactorsString xs
    | otherwise = "(" ++ show (head x) ++ "**" ++ show (length x) ++ ")" ++ primeFactorsString xs

primeFactorsNums :: Integer -> Integer -> [Integer]
primeFactorsNums n m 
    | n < m = []
    | n >= m && (mod n m == 0) = m : primeFactorsNums (div n m) m
    | n >= m = primeFactorsNums n (m + 1)
    | otherwise = []