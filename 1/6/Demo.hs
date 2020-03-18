module Demo where

roots :: Double -> Double -> Double 
            -> (Double, Double)
roots a b c = 
    (
        (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
    ,
        (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
    )
    
roots' a b c =
    let d = sqrt (b ^ 2 - 4 * a * c) in
    ((-b - d) / (2 * a), (-b + d) / (2 * a))
    
roots'' a b c =
    let {d = sqrt (b ^ 2 - 4 * a * c); x1 = (-b - d) / (2 * a); x2 = (-b + d) / (2 * a)}
    in (x1, x2)
    
roots''' a b c =
    let
        x1 = (-b - d) / aTwice
        x2 = (-b + d) / aTwice
        d = sqrt (b ^ 2 - 4 * a * c)
        aTwice = 2 * a
    in (x1, x2)
    
factorial6 n
    | n >= 0  = let
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
      in helper 1 n
    | otherwise = error "arg must be >= 0"
    
rootsDiff a b c = let
    (x1, x2) = roots a b c
    in x2 - x1
    
seqA :: Integer -> Integer
seqA n  | n == 0 = 1
        | n == 1 = 2
        | n == 2 = 3
        | otherwise = helper (n - 2) 1 2 3
    where
        helper n a b c | n == 0 = c
                       | otherwise = helper (n - 1) b c (c + b - 2 * a)





