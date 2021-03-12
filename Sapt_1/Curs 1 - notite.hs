-- O functie de un singur argument

double :: Int -> Int 
double x = x + x       -- singura ecuatie care defineste functia "double"

-- o scrieree de genul "double(10)" functioneaza pt ca are un singur argument, insa in cazul functiilor de argumente ar da probleme, deci este descurajata;
-- se foloseste "double 10", "double (double 10)", etc.,
-- putem scrie si "double (3 * 4 + 9)" sau "double $ 3 * 4 + 9"

-- Haskell este static tipizat si puternic tipizat;
-- Limbaje dinamic tipizate: Python, JavaScript;

double' :: Float -> Float 
double' x = x + x

double'' x = x + x  -- asta merge si pt valori int si pt double
                    
-- Clasa de tipuri = multime de tipuri
--                                              variabila de tip (tine locul unui tip)
--                                                  |
--                                                  v
-- daca dam :type double'' avem: double :: Num a => a -> a
--                               ^^^^^^^^^^^^^^^^^^
--                             constrangere de clasa

--Functie cu mai multe argumente
suma :: Int -> Int -> Int
suma x y = x + y


-- In loc de "3 + 4", se poate scrie "(+) 3 4"


-- Funci recursive
-- Ex: suma primelor n numere naturale 

sum' 0 = 0                      -- linia a
sum' n = n + (sum' (n-1))       -- linia b

-- Cum se calculeaza sum' 10
    -- sum' 10 = 
    --      (lin b)
    -- 10 + (sum' 9)
    --      (lin b)
    -- 10 + (9 + (sum' 8))
    -- ...
    --             
    -- 10 + (9 + (8 + 7 + (6 + (5 + (4 + (3 + (2 + 1 + (sum 0)))))))
    --                                                (lin a)
    -- 10 + (9 + (8 + 7 + (6 + (5 + (4 + (3 + (2 + 1 + (0)))))))


-- Se alege prima ecuatie care se potriveste, astfel, daca inversam liniile de mai sus, va potrivi mereu prima linie "sum' n = n + (sum' (n-1))" chiar si pt 0, si va  merge la infinit