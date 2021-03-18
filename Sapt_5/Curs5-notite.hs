-- Clase de tipuri (typeclasses)
-- Clasa de tipuri = multime de tipuri 
-- Clasa EQ este clasa tuturor tipurilor de date ale caror valori pot fi comparate

-- Un tip 'a' face parte dintr-o clasa de tipuri 'b'
-- Un tip 'a' este o instanta a clasei de tipuri 'b'


data Nat = Zero | Succ Nat deriving Ord

three = Succ (Succ (Succ Zero))
two = Succ (Succ Zero)

-- daca scriem in ghci three == two primi eroare deoarece Nat nu este o instanta a clasei de tipuri Eq;
-- solutia este sa adaugam la sfarsitul definitiei Nat instructiunea deriving (Eq) sau:
instance Eq Nat where
    (==) Zero Zero = True 
    (==) Zero (Succ _) = False 
    (==) (Succ _) Zero = False 
    (==) (Succ x) (Succ y) = (==) x y
    (/=) Zero Zero = False
    (/=) Zero (Succ _) = True
    (/=) (Succ _) Zero =  True
    (/=) (Succ x) (Succ y) = (/=) x y

-- Atentie, in acest caz, comportamentul atat lui == cat si a lui /= functioneaza cum trebuie chiar daca nu definimim comportamentul pentru /= sau pentru ==;
-- Astfel, pt ca Eq sa functioneze, este nevoie sa definim comportamentul doar pentru == sau doar pentru /=;



-------------------------------------------------------------------------------------------------------------------------------------------------------------------

class MyEq a where
    equals :: a -> a -> Bool
    notEquals :: a -> a -> Bool

instance MyEq Nat where
    equals Zero Zero = True 
    equals Zero (Succ _) = False 
    equals (Succ _) Zero = False 
    equals (Succ x) (Succ y) = (==) x y
    notEquals Zero Zero = False
    notEquals Zero (Succ _) = True
    notEquals (Succ _) Zero =  True
    notEquals (Succ x) (Succ y) = (/=) x y

-- MyEq are acelasi comportament la fel ca Eq;
-- In cazul MyEq insa, trebuie sa definim comportamentul atat pentru equals cat si pentru notEquals, SAU definim MyEq' astfel:

class MyEq' a where
    equals' :: a -> a -> Bool
    notEquals' :: a -> a -> Bool
    equals' x y = not (notEquals' x y)
    notEquals' x y = not (equals' x y)

instance MyEq' Nat where
    equals' Zero Zero = True 
    equals' Zero (Succ _) = False 
    equals' (Succ _) Zero = False 
    equals' (Succ x) (Succ y) = (==) x y
    -- notEquals Zero Zero = False
    -- notEquals Zero (Succ _) = True
    -- notEquals (Succ _) Zero =  True
    -- notEquals (Succ x) (Succ y) = (/=) x y

-- Astfel, acum putem omite definitia comportamentului ori pentru equals, ori pentru notEquals;



-------------------------------------------------------------------------------------------------------------------------------------------------------------------

data Z = Pos Nat | Neg Nat --Un dezavantaj, exista doua reprezentari pt Zero

zero1 = Pos Zero
zero2 = Neg Zero

-- Astfel, pt o astfel de definitie, folosind Eq, ne va da ca zero1 este diferit de zero2, ceea ce NU ne dorim;
-- Astfel, va fi nevoie sa definim o instanta a clasei Eq 

instance Eq Z where
    (==) (Pos x) (Pos y) = (==) x y
    (==) (Neg x) (Neg y) = (==) x y
    (==) (Pos x) (Neg y) = (x == Zero) && (y == Zero)   --doua numere cu semn diferit sunt egale doar daca ambele sunt Zero
    (==) (Neg x) (Pos y) = (x == Zero) && (y == Zero)   --doua numere cu semn diferit sunt egale doar daca ambele sunt Zero



-------------------------------------------------------------------------------------------------------------------------------------------------------------------

data Punct2D = Punct Float Float deriving Show

p1 = Punct 0.1 0.1
p2 = Punct 0.09999999 0.1

-- Aici ne dorim sa avem o mica toleranta si deci in cazul de mai sus ne dormi ca cele doua puncte sa fie egale. Insa fara a defini o instanta a clasei Eq, ele vor fi considerate diferite;

instance Eq Punct2D where
    (==) (Punct x1 y1) (Punct x2 y2) = (abs (x1 - x2) <= 0.01) && (abs (y1 - y2) <= 0.01)



 --------------------------------------------------------------------- Clasa Show ----------------------------------------------------------------------------------------------------


data Dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Bounded)

instance Show Dow where
    show Mon = "Luni"
    show Tue = "Marti"
    show Wed = "Miercuri"
    show Thu = "Joi"
    show Fri = "Vineri"
    show Sat = "Sambata"
    show Sun = "Duminica"

-- Deci Show primeste o valoare de tip a (adica orice tip care poate folosi Show) si returneaza un String
-- Deci Show = clasa tipurilor ale caror valori pot fi transformate in String

 --------------------------------------------------------------------- Clasa Read ----------------------------------------------------------------------------------------------------


-- read "True" :: Bool
-- Read primeste un string si returneaza o alta valoare, cumva invers fata de Show
-- Deci e clasa tuturor tipurilor care pot fi convertite din String intr-un acel tip;
-- Deci Read = clasa tipurilor ale caror valori pot fi "chitite" din String



 ----------------------------------------------------------------- Clasa Ord --------------------------------------------------------------------------------------------------------


-- Pt ca un tip de data sa instantieze clasa Ord, este nevoie ca acel tip sa faca parte din clasa Eq si sa definim comportamentul pentru <= sau functia compare;

-- In cazul Dow, daca scriem la sfarsit deriving Ord, atunci ordonarea sa va face implicit in functie de ordinea in care zilele saptamanii au fost specificate;

-- In cazul Z (folosit pt a reprezenta numerele intregi, acel tip cu problema Pos Zero si Neg Zero)
-- Pt ca Z se foloseste de Nat, atunci si Nat ar trebui sa fie o instanta a clasei Ord, sau sa definim noi instanta; daca folosim deriving, comportamentul va fi corespunzator din cauza ordini in care am definit tipul 
instance Ord Z where
    (<=) (Neg _) (Pos _) = True
    (<=) (Neg x) (Neg y) = y <= x -- '<=' de dupa egal va avea comportamentul definit pt instanta clasei Nat
    (<=) (Pos x) (Pos y) = x <= y -- '<=' de dupa egal va avea comportamentul definit pt instanta clasei Nat
    (<=) (Pos x) (Neg y) = (x == Zero) && (y == Zero) -- un nr pozitiv poate fi mai mic sau egal decat un numar negativ doar daca ambele sunt Zero

-- Daca un tip a face parte din clasa Ord, atunci si o lista formata din entitati de tipul a va face parte din clasa Ord; in acest caz, pentru liste se va implementa o ordine lexicografica;
-- La fel si pentru tuple (perechi), unde se implementeaza la fel ordinea lexicografica;

-- Daca a face parte din ord, atunci si Maybe a face parte din clasa Ord;

-- Deci Ord = clasa tipurilor ale caror valori pot fi ordonate



 ------------------------------------------------------------------ Clasa Bounded -------------------------------------------------------------------------------------------------------

-- Clasa tipurilor de date marginite
-- Pt ca un tip sa fie o instanta a clasei Bounded, trebuie sa avem definitiile pt minBound si MaxBound
-- De exemplu, pt Dow, daca scriem cu deriving, atunci minBound va fi luat implicit ca fiind Mon, si maxBound Sun, adica respectand ordinea in care am definit;
-- Daca a face parte din clasa Bounded, iar b face si el parte din clasa Bounded, atunci perechea (a,b) va face si ea parte din calsa Bounded;
-- Deci Bounded = clasa tipurilor ale caror valori pot fi marginite



 ----------------------------------------------------- PARANTEZA - Tipul "()" (citit: tipul unit) -------------------------------------------------------------------------------------------------

--- Tipul "()" (citit: tipul unit) ---

-- Seamana cu tipul "void" in Clasa


 ----------------------------------------------------- PARANTEZA - Tipul Either -------------------------------------------------------------------------------------------------

 -- Un fel de generalizare a lui Maybe;
 
data Poate a = Nimic | Ceva a deriving(Eq, Show, Ord) 

impartire :: Int -> Int -> Poate Int
impartire _ 0 = Nimic
impartire x y = Ceva (x `div` y)

cmmdc :: Int -> Int -> Poate Int 
cmmdc 0 0 = Nimic
cmmdc 0 x = Ceva x
cmmdc x 0 = Ceva x
cmmdc x y = 
    if x == y then Ceva x
    else if x > y then cmmdc y (x-y)
    else cmmdc x (y-x)


-- Aici daca avem o eroare, e mai greu sa ne dam seama daca vine de la o impartire la 0 sau de la un caz netratat

-- Tipul Either are doi constructori Left si Right;
-- Left, prin conventie, cuprinde situatii succesfull, iar Right situatii eronate

data CauzaEroare = ImpartireLaZero | CmmdcNedefinit deriving Show

impartire' :: Int -> Int -> Either Int CauzaEroare
impartire' _ 0 = Right ImpartireLaZero
impartire' x y = Left (x `div` y)

cmmdc' :: Int -> Int -> Either Int  CauzaEroare
cmmdc' 0 0 = Right CmmdcNedefinit
cmmdc' 0 x = Left x
cmmdc' x 0 = Left x
cmmdc' x y = 
    if x == y then Left x
    else if x > y then cmmdc' y (x-y)
    else cmmdc' x (y-x)



 ------------------------------------------------------------------------------------------------------------------------------------------------------

-- Analogie: 
    -- o clasa de tipuri in Haskell != clasa in c++
    -- o clasa de tipuri in Haskell seamana mai degraba cu o interfata in Java


 ----------------------------------------------------------------- Clasa Functor -------------------------------------------------------------------------------------

 -- Kind
 -- Type = tip
 -- Value = valoare

 -- Un kind este pentru tipuri ce este un tip pentru o valoare
 -- ??????????????????????????? reia din inregistrare


 ----------------------------------------------------- PARANTEZA -------------------------------------------------------------------------------------------------
 -- instructiuena type nu defineste un nou tip de date, ci paote fi folosit pt a defini un sinonim de tip
type Intreg = Integer
-- de exemplu string este un sinonim pentru o lista de chiar

data Persoana = P String String deriving Show -- persoana - nume prenume
-- problema este insa ca nu putem forta diferentia intre nume si prenume, adica se pot inversa
-- o solutie ar fi:
data Persoana' = P' { prenume :: String, nume :: String} deriving Show
-- in ghci scriem P' { nume = "Stefan", prenume = "Ciobaca" }
-- aceasta treaba se numeste record syntax