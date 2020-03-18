module Demo where

getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom x y z = y

mono :: Char -> Char
mono x = x

semiMono :: Char -> a -> Char
semiMono x y = x

apply2 f x = f (f x)






