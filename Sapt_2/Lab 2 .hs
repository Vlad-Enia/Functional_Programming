--- Exercitii Lab 2: Enia Vlad Ieftimie 2E4 (2PF3) ---

-- 1).

myand :: Bool -> Bool -> Bool
myand False _ = False
myand _ False = False 
myand _ _ = True 

myor :: Bool -> Bool -> Bool
myor True _ = True 
myor _ True = True 
myor _ _ = False 

mynot :: Bool -> Bool 
mynot False = True 
mynot True = False 

mynand :: Bool -> Bool -> Bool
mynand False _ = True
mynand _ False = True 
mynand _ _ = False 

mynor :: Bool -> Bool -> Bool
mynor True _ = False 
mynor _ True = False 
mynor _ _ = True 

myimpl :: Bool -> Bool -> Bool
myimpl a b = myor (mynot a) b  

mydimpl :: Bool -> Bool -> Bool
mydimpl a b = myand (myimpl a b) (myimpl b a)


-- 2).

hasDivisors :: Integer -> Integer -> Integer -> Bool -- Returneaza true daca primul parametru are vreun divivizor din intervalul format de cel de-al doilea si al treilea parametru
hasDivisors n a b | a > b = False 
hasDivisors n a b | n `mod` a == 0 = True 
hasDivisors n a b = hasDivisors n (a + 1) b

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = mynot (hasDivisors n 2 (floor (sqrt (fromInteger  n ) ) ) )

-- 3).

cmmdcEuclid :: Integer -> Integer -> Integer
cmmdcEuclid x y
    | x == y = x
    | x > y = cmmdcEuclid (x - y) y
    | x < y = cmmdcEuclid x (y - x)

cmmdcEuclid' :: Integer -> Integer -> Integer
cmmdcEuclid' x 0 = x
cmmdcEuclid' x y = cmmdcEuclid' y (x `mod` y)

cmmdcBin :: Integer -> Integer -> Integer 
cmmdcBin 0 y = y
cmmdcBin x 0 = x
cmmdcBin x y 
    | even x && even y = 2 * cmmdcBin (x `div` 2) (y `div` 2)
    | even x && odd  y = cmmdcBin (x `div` 2) y
    | odd  x && even y = cmmdcBin x (y `div` 2)
    | odd  x && odd  y = cmmdcBin (abs (x - y)) (min x y)


-- 4). O optimizare posibila pentru a aduce apelurile recursive in pozitie de coada este reprezentata de utilizarea unu acumulator, permitand astfel compilatorului sa renunte la folosirea unei stive.

-- 5). 
fibo :: Integer -> Integer 
fibo 0 = 0
fibo 1 = 1
fibo x = fibo(x-1) + fibo(x-2)

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n-1) b (a + b)

fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1

fibolog :: Integer -> Integer
fibolog 0 = 0
fibolog 1 = 1
fibolog 2 = 1
fibolog n =
    if even n
        then let k = n `div` 2 in (2 * fibolog (k - 1) + fibolog k) * fibolog k
    else let k = (n + 1) `div` 2 in (fibolog k * fibolog k + fibolog (k-1) * fibolog (k-1))


-- 6).
gcdext :: Integer -> Integer -> Integer
gcdext 0 b = b
gcdext a b = gcdext (b `mod` a) a


-- 7). 
mysucc :: Integer -> Integer
mysucc x = x + 1

-- 8).
myadd :: Integer -> Integer -> Integer 
myadd a 0 = a
myadd a b = myadd (succ a) (b-1)

mymult :: Integer -> Integer -> Integer
mymult 0 _ = 0
mymult _ 0 = 0
mymult 1 b = b
mymult a 1 = a
mymult a b = myadd a (mymult a (b - 1))

mypwr :: Integer -> Integer -> Integer
mypwr 0 _ = 0 
mypwr _ 0 = 1
mypwr 1 _ = 1
mypwr a 1 = a
mypwr a b = mymult a (mypwr a (b - 1))

-- 9).
mydiv :: Integer -> Integer -> Integer
mydiv 0 _ = 0
mydiv a b  
    | a==b = 1
    | a<b = 0
    | a>b = 1 + mydiv (a-b) b

mymod :: Integer -> Integer -> Integer
mymod _ 1 = 0
mymod a b 
    | a==b = 0
    | a<b = a
    | a>b = mymod (a-b) b