------------------------------------------------------------------------------------ Module (in general, cum organizam un proiect in Haskell) ------------------------------------------------------------------------------------

-- Un modul este similar cu un header + impl in C++
-- De regula intr-un proiect Haskell exista un fisier principal, care contine functia main.

import qualified Nat as N
import qualified Utils.Intreg as I

-- data Nat = Zero | Succ Nat deriving Show

-- add :: Nat -> Nat -> Nat
-- add Zero x = x
-- add (Succ y) x = Succ (add y x)

-- Ne dorim sa fim organizati si sa separam ceea ce tine de Nat de partea Main
-- Asa ca cream fisierul nat.hs in care vom defini modulul Nat

main :: IO ()
main = 
    do
        putStrLn "Hello World!"
        print (N.add (N.Succ (N.Succ N.Zero)) (N.Succ (N.Succ (N.Succ N.Zero))))



-- Pentru a defini un modul intr-un alt fisier: import NumeFisier.NumeModul
-- Daca scriu "import Utils.Intreg", toti indentificatorii din modulul Utils.intreg sunt adusi in spatiul de nume curent, ceea ce poate duce la coloziuni,
-- cum avem in cazul de fata coliziunea dintre functia add din Intreg si add din Nat
-- O solutie ar fi sa scri "import qualified Utils.Intreg as I", si atunci inainte de a folosi orice din Intreg, trebuie precedat cu "I."


-- Rezumat
    -- fiecare modul trebuie sa mearga intr-un file cu acelsi nume
    -- daca organizez modulele pe subdirectoare, numele module trebuie prefixate cu calea catre fisierul care contine modulul
    -- "qualified" poate fi folosit pentru a nu "polua" spatiul de nume
    -- sintaxa "module (id1, id2, ...) where" poate fi folosita pentru a exporta din modul doar anumiti identificatori



------------------------------------------------------------------------------------ Tool-ul Stack ------------------------------------------------------------------------------------

-- Folosit pentru a mentiona versiunea de compilator si pachetele folosite
-- Poate fi folosit pentru a creea noi proiecte dupa un anumit template



------------------------------------------------------------------------------------ Propierty-based testing ------------------------------------------------------------------------------------

-- Functia reverse ne inverseaza o lista
reverse_prop1 :: [Int] -> Bool 
reverse_prop1 l = l == (reverse (reverse l))

-- Biblioteca QuickCheck contine o functie care primeste o proprietate (adica o functie care returneaza bool), si verifica daca aceassta este adevarata
-- Instalam QuickCheck folosind stack: stack ghci --package QuickCheck
-- QuickCheck ne ruleza mai multe teste pana cand gaseste unul care pica


qs :: [Int] -> [Int]
qs [] = []
qs (hd : tl) = (filter (<=hd) tl) ++ [hd] ++ (filter (>hd) tl) -- un QuickSort gresit

sorted :: [Int] -> Bool 
sorted [] = True 
sorted (hd:[]) = True 
sorted (hd1:(hd2:tl)) = (hd1 <= hd2) && sorted (hd2:tl)

qs_prop1 :: [Int] -> Bool 
qs_prop1 l = sorted (qs l)
-- proprietate care verifica ca functia este sortata

qs_prop2 :: [Int] -> Bool 
qs_prop2 l = length l == length (qs l)
-- propritetate care verifica ca nu s-au pierdut sau adauggat elemente la lista initiala dupa ce aceaasta a fost sortata



data Nat = Zero | Succ Nat deriving (Show, Eq)

convert :: Nat -> Int 
convert Zero = 0
convert (Succ x) =  (convert x) + 1

convert' :: Int -> Nat
convert' x | x<0 = error "Numar negativ!"
convert' 0 = Zero
convert' x = Succ (convert' (x-1))

convert_prop1 :: Nat -> Bool 
convert_prop1 n = convert' (convert n) == n
-- Aici nu merge pt ca nu stie sa genereze elemente de tip Nat aleatoriu
-- Trebuie sa facem Nat instanta a lui Arbitrary

convert_prop2 :: Int -> Bool 
convert_prop2 n = (n<=0) || convert (convert' n) == n -- dorim sa ignoram cazurile in care numerul generat pentru a fi testat este negativ