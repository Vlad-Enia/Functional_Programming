---- CURS 2 ----

-- 1). Functii Recursive

    -- Transparenta referentiala 
        -- Doua bucati de cod identice -> acelasi rezultat, indiferent de contexul in care sunt evaluate.
        -- De exemplu, in C, daca avem o functie in care se efectueaza operatii asupra unei variabile globale, atunci apelul functiei in main() poate da rezultate diferite in functiie de valoarea variabilel globale

    -- In Haskell nu avem bucle for/while, asa ca ne vom folosi de functii recursive;

    -- Exemplu - functie care returneaza al n-lea nr din sirul lui fibonacci

    fibo :: Integer -> Integer
    fibo 0 = 1
    fibo 1 = 1
    fibo n = fibo (n-1) + fibo (n-2)

    -- Exmeplu - ecuatii constranse de GARZI
    fibo' :: Integer -> Integer 
    fibo' :: x | x==0 || x==1 = 1    -- | x == 0 || x == 1 (contitie logica pentru ca egalitatea de la sfarsit sa aiba loc)
    fibo' :: x                = fibo'(n-1) + fibo'(n-2)

    fibo'' :: Integer -> Integer
    fibo'' x | x==0 = 1 
    fibo'' x | x==1 = 1 
    fibo'' n        = fibo' (n-1) + fibo' (n-2)


    -- Exemplu cmmdc
    cmmdc :: Integer -> Integer -> Integer  -- nr naturale, nu ambele 0
    cmmdc x 0 = x
    cmmdc 0 y = y
    cmmdc x y = if x > y then cmmdc(x-y,y)
                else cmmdc(x,y-x)


    -- Exemplu suma primelor n nr naturale - recursiv, nu e chiar foarte eficient
    sumn :: Integer -> Integer -> Integer
    sumn 0 = 0
    sumn n = n + sumn(n-1)
--              ^^^^^^^^^^
--              apel recursiv care nu e in pozitie de coada 


-- 2). Functii cu ACUMULATORI
    sumna :: Integer -> Integer -> Integer 
    sumna 0 a = a 
    sumna n a = sumna (n-1) (n+a)   -- ~~~~> apelurile recursive in pozitie de coada pot fi transformate de compilator in bucle mai eficiente, deoarece nu este nevoie de stiva
--              ^^^^^^^^^^^^^^^^^
--              apel recursiv in pozitie de coada (tail-call)

-- 3). Avantaj transparente referentiale + rationament ecuational.

    -- "sumna" calculeaza acelasi lucru cu "sumn"?
    -- Cu alte cuvinte, sumna n a = a + sumn n
    -- Folosim rationamentul ecuational pt a demonstra acest lucru:
        -- Inductie dupa n;

        -- pt n=0: sumna 0 a = a; sumn 0 = 0 
        
        -- pt n>0: 
            -- sumna n a = sumna (n-1)(n+a) = (n+a) + sumn(n-1) = (a+n) + sumn(n-1) = a + (n + sumn(n-1)) = a + sumn nu

-- 4). Tipul de date PERECHI
    -- Pereche = agregarea a doua valori de tipuri potential diferite
    -- Exemple: (12, 13), (12, 'A'), (True, 'C');
    -- fst('A',13) -> 'A'
    -- snd(True, False) -> False 

    myfst :: (Integer, Integer) -> Integer 
    myfst (x,y) = x + y

    mysnd :: (Integer, Integer) -> Integer 
    mysnd :: (_, y) = y

    mysum :: (Int, Int) -> Int 
    mysum x = fst x + snd y

    mysum' :: (Int, Int) -> Int 
    mysum' :: (x,y) = x+y

    -- Toate functiile de mai sus primesc un singur argument : o pereche

-- 4). Liste 
    -- [1, 4, 7, 8]
    -- [True, False, Bool]
    -- Listele contin elemente de aceelasi tip
    -- :type [False, True, False] :: [Bool]
    -- Functii : head, tail, length
    sumlista :: [Integer] -> Integer 
    sumlista [] = 0
    --       ^^ lista goala
    sumlista (hd:tl) = hd + sumlista tl
    --                      ^^^^^^^^^^^ nu e tail recursive

    -- sumlista [1,2,3] -> (ecuatia 2)
    -- = 1 + sumlista [2,3] -> (ecuatia 2)
    -- = 1 + (2 + sumlista[3]) -> (ecuatia 2)
    -- = 1 + (2 + (3 + sumlista [])) -> (ecuatia 0)
    -- = 1 + (2 + (3 + 0))