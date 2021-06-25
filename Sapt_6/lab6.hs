------------------------ Laborator 6 ------------------------

--- Ex.1 ---
qs :: Ord a => [a] -> [a]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)

myMin :: Ord a => [a] -> a
myMin x = (qs x) !! 0


--- Ex.2 ---

-- qs [1..10000] --> (17.93 secs, 2,839,973,504 bytes)
-- length (qs [1..10000]) --> (15.23 secs, 2,805,772,824 bytes)
-- myMin [1..10000]  --> (0.02 secs, 770,616 bytes)


--- Ex.3 ---

fib :: Int -> Int 
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibLista :: Int -> [Int]
fibLista x = fib x : fibLista(x + 1)


--- Ex.4 ---

isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] 
            else False

primListaBool :: Int -> [Bool]
primListaBool x = (isPrime x) : (primListaBool (x+1))


--- Ex.5 ---

primLista :: Int -> [Int]
primLista x = if isPrime x then x : (primLista (x+1))
              else primLista (x+1)


--- Ex. 6 ---

data Lista a = Vida | Cons a (Lista a) deriving (Show, Eq)

addBeginningLista :: Lista a -> a -> Lista a
addBeginningLista Vida x = Cons x Vida
addBeginningLista l x = Cons x l

addEndLista :: Lista a -> a -> Lista a
addEndLista Vida x = Cons x Vida
addEndLista (Cons y next) x = Cons y (addEndLista next x)

indexLista :: Lista a -> Int -> Int -> a 
indexLista (Cons x next) i1 i2 = if (i1 == i2) then x
                                 else indexLista next (i1+1) i2 

atIndexLista :: Lista a -> Int -> a
atIndexLista (Cons x next) i = indexLista (Cons x next) 0 i

--- 6.3 ---

fibLista' :: Int -> Lista Int
fibLista' x = Cons (fib x) (fibLista' (x+1))


--- 6.4 ---

primListaBool' :: Int -> Lista Bool
primListaBool' x = Cons (isPrime x) (primListaBool' (x+1))


--- 6.5 ---

primLista' :: Int -> Lista Int 
primLista' x = if isPrime x then Cons x (primLista' (x+1))
               else primLista' (x+1)


--- 6.1 ---

append :: Lista a -> Lista a -> Lista a
append Vida l = l
append (Cons x xs) l = Cons x (append xs l)

myFilter :: (a->Bool) -> Lista a -> Lista a
myFilter f Vida = Vida
myFilter f (Cons x xs) = if f x then Cons x (myFilter f xs)
                       else myFilter f xs

qs':: Ord a => Lista a -> Lista a
qs' Vida = Vida
qs' (Cons hd tl) = append (append (qs' (myFilter (<=hd) tl)) (Cons hd Vida)) (qs' (myFilter (>hd) tl))

myMin' :: Ord a => Lista a -> a
myMin' l = atIndexLista (qs' l) 0


--- 6.2 ---

generate :: Int -> Int -> Lista Int 
generate x y = 
    if x == (y+1) then Vida
    else Cons x (generate (x+1) y)

myLength :: Lista a -> Int 
myLength Vida = 0
myLength (Cons _ xs) = 1 + (myLength xs)


-- let list = generate 0 10000 in qs' list  -->  (50.40 secs, 19,308,579,464 bytes)
-- let list = generate 0 10000 in myLength(qs' list)   -->  (39.13 secs, 19,214,414,632 bytes)
-- let list = generate 0 10000 in myMin' list  -->  (0.01 secs, 4,691,360 bytes)