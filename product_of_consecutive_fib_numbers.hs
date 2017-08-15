-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci
--   numbers.
productFib :: Integer -> (Integer, Integer, Bool)
productFib n = locationProductFibN n 2

-- i must >= 2
locationProductFibN :: Integer -> Integer -> (Integer, Integer, Bool)
locationProductFibN n i
  | fib (i-1) * fib (i) == n = ( fib (i-1) , fib (i), True)
  | fib (i) * fib (i+1) > n = ( fib (i) , fib (i+1) , False)
  | otherwise = locationProductFibN n (i+1)



fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
