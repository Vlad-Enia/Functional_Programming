--- Curs 3 PF --- Notite ---------------------------------------------------------------

-- Functiile care primesc sau care intorc alte functii (de ordin superior)

add42 :: Integer -> Integer --de ordin 0
add42 x = x+42  -- linia (B)

mult3 :: Integer -> Integer --de ordin 0
mult3 x = x*3

---------------------------------------------------------------------------------------------------------------------------

--[EXEMPLU] - functie care primeste functie ca argument:

--Am sa scriu o functie numita "twice" care primeste doi parametri:
--      o functie "f"
--      un umar "x"
--functia twice apeleaza f de 2 ori pe x;

--twice:: functii-de-la-Integer-la-Integer -> Integer -> Integer
twice  ::     (Integer -> Integer)         -> Integer -> Integer
twice f x = f (f x) -- linia (A)

-- twice add42 3 = 
--                  (A)
-- add42 (add42 3)
--                  (B)
-- add42 (3 + 42)
--                  (B)
-- (3+42) + 42 = 87

--[OBS] Un singur lucru nu putem face cu functii: nu le putem transforma in String-uri;

---------------------------------------------------------------------------------------------------------------------------

--[EXEMPLU] Exemplu de functie care intoarce o functie ca rezultat:

--makeAdder c = functie-care-aduna-c-la-argumentul-ei
makeAdder c = \x -> x + c   -- o functie care primeste un nr intreg, si intoarce o functie ca rezultat
--            ^^^^^^^^^^^^
--            functia care are un argument x si intoarce x + c

add7' = makeAdder 7 -- functia rezultata, intoarsa de makeAdder, pe argumentul 13
add13' = makeAdder 13
-- putem creea nou functii direct in ghci, de exemplu un add8', scriind (makeAdder 8)
-- ca sa folosim add7', scriem add7' x, iar rezultatul va di un numar egal cu 7+x;

---------------------------------------------------------------------------------------------------------------------------
-- :t makeAdder :: Num a = a -> a -> a
-- asadar, ii putem da doua argumente, desi noi am stabilit la ineput ca nu;
-- de exemplu, putem scrie in ghci makeAdder 13 17, si ne va returna 30;

-- In Hasqell, nu exista, oficial, functii cu mai multe argumente.
-- Orice functie are exact 1 argument (*);
-- (*) sunt si functii cu 0 argumente;

-- Intr-un tip de forma a1 -> a2 -> .. -> an (fara paranteze), sistemul asociaza automat paranteze la dr;
-- Exemplu, tipul a1 -> a2 -> a3 este acelasi tu tipul a1 -> (a2 -> a3) 

mysum :: Integer -> (Integer -> Integer)
mysum x y = x + y

secret1 = mysum 7;
-- :t secret1 :: Integer -> Integer
-- secret1 200 = 207
-- deci functia secret1 incrementeaza argumentul cu 7

-- Intr-o secventa de forma f x1 x2 x3, sistemul asociaza automat paranteze spre stanga
-- de ex, f x1 x2 x3 este acelasi lucru cu ((f x1) x2) x3.

--Metoda de a defini neoficial functii cu mai multe argumente ca o secventa de functii cu un singur argument se numeste curry-ing

--de exemplu, in loc sa scriem (+) 12 17, care e egal cu 29, putem scrie secret2 = (+12), si apoi secret2 17, care va avea acelasi rezultat;

-- un alt ex de curry-ing

--   un singur arg: o pereche de Integeri
--   vvvvvvvvvvvvvvvvvv
f :: (Integer, Integer) -> Integer -- varianta apropiata de limbaje obisnuite
f (x, y) = x + y

g :: Integer -> (Integer -> Integer) -- este varianta "curried" a functie f
g x y = x + y

{-
h :: (Integer -> Integer -> Integer) -> Integer
h (x, y, z) = x + y + z
-}

h' :: Integer -> (Integer -> (Integer -> Integer)) -- varianta curried
h' x y z = x + y + z

--putem apela h' 13 13 14, sau ((h' 13) 13) 14


----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------


secret4 = (.) add42 mult3
-- (.) este compunerea a doua functii
-- (.) add42 mult3 are urmatorul comportament -> inmulteste cu 3, si apoi adauga 42
-- (.) mult3 add 42 are comportament invers

compunere f g = \x -> f (g x) -- functie la fel cu (.) 

-- (.) se poate scrie si functie1 . functie2, care e echival (.) functie1 functie2



-----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------

-- :t ($) ne da (a -> b) -> a -> b 
-- ($) este aplicarea unei funtii
-- putem folosi in cazul in care vrem sa scriem add 42 (add 42 (add42 5)), scriem astfel add42 $ add42 $ add42 $ 5
-- tot ce e in dreapta $ este folosit ca paramtru pt ce este in stanga

aplicare :: (a->b) -> a -> b
aplicare f x = f x
-- aplicare add42 (aplicare add42 (aplicare add42 5))
--[OBS] programatorul poate defini proprii operatori infix

-----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------

-- Functiile de ordin superior sunt utile pentru tipurile de date parametrizabile


aduna7 :: [Integer] -> [Integer]
aduna7 [] = []
aduna7 (x:xs) = x + 7 : (aduna7 xs)

aduna3 :: [Integer] -> [Integer]
aduna3 [] = []
aduna3 (x:xs) = x + 7 : (aduna3 xs)

--functiile sunt foarte asemanatoare, asa ca facem astfel

aduna :: Integer -> [Integer] -> [Integer]
aduna _ [] = []
aduna c (x:xs) = x + c : (aduna c xs)

-- si deci
aduna3' = aduna 3 -- exact aduna3
aduna7' = aduna 7 -- exact aduna7



times8 :: [Integer] -> [Integer]
times8 [] = [] 
times8 (x:xs) = x*8 : (times8 xs)

times :: Integer -> [Integer] -> [Integer]
times _ [] = []
times c (x:xs) = x * c : (times c xs)

-- ovservam ca aduna si time sunt asemanatoare, asa ca vrem sa cream o functie care poate indeplini scopul la ambele, sau chiar si alte operatii

proceseaza :: (Integer -> Integer) -> [Integer] -> [Integer]
proceseaza _ [] = [] 
proceseaza f (x:xs) = f x : (proceseaza f xs)

-- si deci putem apela proceseaza add42 [1,2,3], care ne returneaza [43, 44, 45]
-- putem scrie si direct functia in ghci: proceseaza (\x -> x ^ 2 ) [1, 2, 3] care ne returneaza [1, 4, 9]

-- putem folosi functia map astfel: map (\x -> x ^ 2) [1, 2, 3] care ne returneaza [1, 4, 9]
-- map e putin mai general decat proceseaza, deoarece proceseaza merge doar pe Integer si returneaza Integer
-- map (\x -> x `mod` 2 == 0) [1, 2, 3] ne returneaza [False, True, False]


-- functia filter primeste un predicat boolean si o lista si  returnea o alta lista
-- filter (\x -> x `mod` 2 == 0) [1, 2, 3, 4, 5, 6, 7] ne returneaza [2, 4, 6]
filtrare :: (a -> Bool) -> [a] -> [a]
filtrare _ [] = []
filtrare p (x:xs) = 
    let xs' = filtrare p xs in
        if p x then
            x : xs'
        else
            xs'

-- Deci map primeste o lista, si ne returneaza o lista cu acelasi nr de elemente
-- Filter primeste o lista, si returneza lista continand doar elementele ce indeplinesc conditia booleana
-- Reduce primeste o lista si ne returneaza doar un element

reduce :: (b -> a -> b) -> b -> [a] -> b
reduce _ init [] = init
reduce f init (x:xs) = reduce f (f init x) xs

-- reduce mysum 0 [1,2,3,4] ne da 10 -- deci suma tuturor elementelor din lista
-- reduce (*) 1 [1,2,3,4,5] ne da 120 -- deci produsul tuturor elementelor din lista
-- reduce (\x y -> x) 42 [1, 2, 3] ne returneaza 42 (ia 42 si 1, ignora pe 1 si returneaza 42, apoi ia 42 si 2, si reeturneaza tot 42, si deci ia 42 si 3, si returneaza tot 42)
-- reduce (\x y -> y) 42 [1, 2, 3] ne returneaza 3 (ia 42 si 1 si da 1, ia 1 si 2 si da 2, ia 2 si 3 si da 3)

-- foldl si foldr ne returneaza o sumarizare a elementelor dintr-o lista
-- avem lista [x1, x2, ..., xn], si vrem sa obtinem suma tuturor elementelor
-- foldr (+) i [x1, x2, ..., xn] -> x1 (x2 + (x3 + (... (xn-1 + (xn+i)))))
-- flodl (+) i [x1, x2, ..., xn] -> ((((((i + x1) + x2) + x3) + ...) + xn)