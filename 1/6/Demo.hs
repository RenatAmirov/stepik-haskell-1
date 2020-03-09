module Demo where

factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

factorial'' 0 = 1
factorial'' n = if n < 0 then error "arg must be >= 0"  else n * factorial'' (n - 1)

factorial''' 0 = 1
factorial''' n | n < 0 = error "arg must be >= 0"
               | n > 0 = n * factorial''' (n - 1)

factorial4 :: Integer -> Integer
factorial4 n | n == 0    = 1
             | n > 0     = n * factorial4 (n - 1)
             | otherwise = error "arg must be >= 0"

doubleFact :: Integer -> Integer
doubleFact n | n <= 0   = 1
             | n > 0    = n * doubleFact (n - 2)

fibonacci :: Integer -> Integer
fibonacci n | n < 0  = fibonacci (n + 2) - fibonacci (n + 1)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 1  = fibonacci (n - 1) + fibonacci (n - 2)

factorial5 n | n >= 0     = helper 1 n
             | otherwise  = error "arg must be >= 0"

helper acc 0  = acc
helper acc n  = helper (acc * n) (n - 1)

fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n | n < 0  = ((-1) ^ (n + 1)) * (helper' 1 1 ((-n) - 2))
             | n > 1  = helper' 1 1 (n - 2)

helper' prev current 0  = current
helper' prev current n  = helper' current (prev + current) (n - 1)

fibonacci'' :: Integer -> Integer
fibonacci'' n = helper'' n 0 1
helper'' n a b | n == 0 = a
               | n > 0  = helper'' (n - 1) b (a + b)
               | n < 0  = helper'' (n + 1) b (a - b)
               | otherwise = undefined




