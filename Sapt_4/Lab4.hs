--- Lab 4 ---

-- 1.1
-- Putem scrie fara paranteze si atunci ele se vor asocia in mod implicit la dreapta, sau putem sa punem noi parantezele astfel:

addThree:: Int -> (Int -> (Int -> Int))
addThree x y z = x + y + z;


-- 2.1
f21 :: (Int -> Int) -> Int -> Int -> Int 
f21 f x y | x ==  y = f x
          | x  <  y = f x + f21 f (x+1) y

dublu :: Int -> Int
dublu x = 2 * x



-- 2.2
comp :: (t2 -> t3) -> (t1 -> t2) -> t1 -> t3
comp f g x = f (g x)

checkEven :: Int -> Bool 
checkEven x | even x = True
            | odd x = False

inc :: Int -> Int 
inc x = x + 1


-- 2.3
comp' :: [Int -> Int] -> Int -> Int 
comp' [] x = x
comp' (f:fs) x = comp' fs (f x)         -- aici compunerea ar urma un curs asemanator cu un foldl, adica parantezele sunt asociate la stanga, deci prima functie apelata pe x va fi f1
                                        -- comp [f1, f2, ... fn] x = fn(...(f2 (f1 (x))))

-- 2.4
sumList :: [Int] -> Int -> Int
sumList [] a = a
sumList (x:xs) a = sumList xs (a+x)

sumList' :: [Int] -> Int -> Int 
sumList' [] a = a
sumList' xs a = foldl (+) a xs

sumList'' :: [Int] -> Int 
sumList'' [] = 0
sumList'' (x:xs) = x + sumList'' xs