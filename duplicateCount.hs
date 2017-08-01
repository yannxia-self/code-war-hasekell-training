import Data.Char
import Data.List

duplicateCount :: String -> Int
duplicateCount input = length $ filter (\x -> length x > 1 ) groupStr
  where lowerStr = map toLower input
        groupStr = group . sort $ lowerStr

