oddOrEven :: [Num] -> String
oddOrEven xs = if odd $ sum xs then "odd" else "even"