------------------------------------ Polimorfism ------------------------------------

data Lista a = Vida | Cons a (Lista a) deriving (Eq, Show) -- tip de date polimorfic
-- liste de elemente de tip a

-- polimorfism parametric - functia trateaza in mod uniform toate aceste valori de tip a (ex. acelasi lucru pe care il face pt Int il face si pt Bool)
lungime :: Lista a -> Int 
lungime Vida = 0
lungime (Cons x tl) = 1 + lungime tl


-- polimorfism ad-hoc - pt fiecare tip "a", se foloseste o anumita operatie
-- qs :: [a] -> [a] --asa nu putem scrie deoarece nu stim nimic despre a, in acest caz nu stim cum sa comparam 2 obiecte de tip a 
qs :: Ord a => [a] -> [a]
qs [] = []
qs (hd:tl) = qs (filter (<=hd) tl) ++ [hd] ++ qs (filter (>hd) tl)          --- pt (<=hd) nu am facut nimic altceva decat sa folosim varianta prefixa a lui <=, si sa fixam un parametru 

-- polimorfismul parametrice este un instrument puternic pt rationamente despre cod

-- ex. 1
ex1 :: a -> b
-- singura functie cu signatura de mai sus este functia care are un loop infit pt orice parametru
ex1 x = ex1 x

-- ex. 2
ex2 :: a -> a
-- doua optiuni:
--      functia care bucleaza la infinit
--      functia identitate:
ex2 x = x

--ex3 :: [a] -> a
-- optiuni:
--      primul element
--      al doilea element
--      ...
--      ultimul element
--      sa intorc al n lea element (in functie de lungimea listei)
--      sa merg la infinit pt anumite lungimi de lista, dar sa returenz un al n-lea element pt anumite lungimi

-- Teorema:
-- ex3 [1,2,3] == 2 ==> ex3 ['a','b','c'] == 'b'
-- Este imposibil sa scriu o functie ex3 :: [a] -> a care sa satisfaca ex3 [1,2,3] == 2 dar sa nu satisfaca ex3 ['a','b','c'] = 'b'

-- Rezumat
    -- sa stim sa diferentiem intre poli parametric si poli ad hoc
    -- cu poli parametric putem face rationamente de functie doar citind signatura ei




------------------------------------ Evaluare lenesa ---------------------------------------

-- Reamintim ce am invatat la liceu despre C/C++
    -- Pentru executia unei instructiuni de atribuire "x = expresie;"
        -- 1. Se evalueaza mai intai rezultatul expresiei, iar apoi
        -- 2. Se memoreaza in variabila x.
    -- Pentru apelul unei functii f(exp1, ..., expn), 
        -- 1. Se evalueaza mai intai fiecare expresie expi (1 <= i <= n), iar poi 
        -- 2. Functia este apelata propriu zis

-- Aceste fenomene se intampla in limbaje cu "eager evaluation" - evaluare "cat mai devreme";

fib :: Int -> Int 
fib 0  = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)   --(*)

-- Experimentul 1
a :: Int 
-- a = let x = 123 in x + 2  rezultatul va fi 125
a = let x = 123 in 2       --rezultatul va fi 2

-- a = let x = fib 100 in 2  rezultatul va fi 2 si se face instant, chiar daca ne asteptam ca fib 100 dureaza foarte foarte mult

-- Experimentul 2
b :: Int 
b = let x = fib 100 in
    let y = 13 in
    if y > 10 then 2 else x     -- rezultatul va fi tot 2, deci nu se mai evalueaza fib 100

-- Experimentul 3
f :: Int -> Int -> Int 
f x y = if x > 2 then y else x  -- (**) la un apel de genul "f 1 (fib 100), la un limbaj eager s-ar fi evaluat parametrii iar apoi se efectueaza apelul, insa in haskell daca dam asa atunci ne returneaza 1

-- Varianta eager:
    -- f 1 (fib 100)=(*)
    -- f 1 (fib 99 + fib 98)=
    -- ...


-- if True then x else y = x    (3*)
-- if False then x else y = y   (4*)


-- Varianta lazy
    -- f 1 (fib 100) = (**)
    -- if 1 > 2 then (fib 100) else 1 = (definitie >)
    -- if False then (fib 100) else 1 = (4*)
    -- 1

-- Deci expresie se apeleaza doar daca este nevoie de ele


ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y
-- o implementare pt If Then Else

-- fiecare variabila in Haskell nu contine o valiare, ci o bucata de cod care, daca eeste apelata, calculeaza propriu zis valoarea.

listaAux :: Int -> [Int]
listaAux i = i : (listaAux (i+1))

listaNat :: [Int]
listaNat = listaAux 0       -- (E5)

-- Mai sus practic am definit o structura infinita;

-- (!!) :: [a] -> Int -> a
-- (!!) (x:xs) 0 = x                (E1)
-- (!!) (x:xs) n = (!!) xs (n-1)    (E2)

-- map :: (a->b) -> [a] -> [b]
-- map f [] = []                    (E3)
-- map f (x:xs) = (f x) : map f xs  (E4)

-- Fara evaluare lenesa:
-- (map (+13) listaNat) !! 0   - nu s-ar opri
    -- mai intai se executa o infinitate de pasi, si apoi ne da elementu de la index 0

-- Cu evlauare lenesa
-- (map (+13) listaNat) !! 0 = (E5)
-- (map (+13) (listaAux 0)) !! 0 = (E6)
-- (map (+13) (0 : (listaAux (0 + 1)))) !! 0 = (E4)
-- ((+13) 0 : map (+13) (listaAux (0 + 1)))) !! 0 = (E1)
-- (+13) 0 = (def. +)
-- 13

-- Evaluarea lenesa este cruciala pentru a implementa programe cu stare
-- citire/scriere din fisier/tastatura;
-- retea

-- Nu exista niciun limbaj *pur* functuional fara evaluare lenesa, dar cu posibilitatea de interactiune cu mediul (fisiere/tastatura/retea);

