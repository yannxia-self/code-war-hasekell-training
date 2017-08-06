-- | All numbers in the unsorted list are present twice,
--   except the one you have to find.
import Data.List

-- | All numbers in the unsorted list are present twice,
--   except the one you have to find.
findUnique :: [Int] -> Int
findUnique xs = head $ head $ filter (\x -> length x == 1) $ group $ sort xs