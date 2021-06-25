-- Universitatea Alexandru Ioan Cuza
-- Facultatea de Informatica
-- Simulare Examen Programare Functionala 2020-2021
-- 8 Aprilie 2021

-- Salvati fisierul cu numele nume_prenume_grupa.hs, completati
-- raspunsurile si incarcati-l aici:
-- https://www.dropbox.com/request/yfM5ojwOhJ7X6VViQDcf.

-- Multe dintre exercitii suporta solutii deschise -- va puteti folosi
-- creativitatea cat de multi doriti. Acolo unde exercitiile sunt
-- subspecificate, puteti face alegeri rezonabile pe care sa le
-- documentati.




-- Date identificare student
-- PRENUME: Vlad Ieftimie
-- NUME:    Enia
-- GRUPA:   E4

import Data.Char

-- Exercitiul I

-- Scrieti o functie recursiva care primeste la intrare un sir de
-- caractere nevid si returneaza primul cuvant din acel sir.  Prin
-- cuvant intelegem un sir de caractere fara spatii. Puteti defini
-- propriile functii ajutatoare, dar nu puteti utiliza functii din
-- biblioteca standard (exceptii: puteti utiliza ++, if-then-else,
-- ==).

-- Solutie

-- am definit aceasta functie pentru a acoperi cazurile in care sirul de caractere incepe cu unul sau mai multe spatii, astfel incat acestea sa fie ignorate;
-- astfel, functia "spacetrim" elimina toate spatiile de la inceputul sirului de caractere, daca acestea exista;

spacetrim :: String -> String
spacetrim [] = []
spacetrim (x:xs) =
    if([x] == " ") then (spacetrim xs)  -- "sarim" peste spatii pana cand
    else [x] ++ xs                      -- intalnimul caracter care nu este spatiu, moment in care afisam sirul incepand cu acel caracter 


-- functia fstwordaux este cea care afiseaza primul cuvant dintr-un sir de caractere dat

fstwordaux :: String -> String 
fstwordaux [] = []
fstwordaux (x:xs) = 
    if([x] == " ") then []      -- in momentul in care intalnim un spatiu, afisam sirul vid, oprind totodata exacutia functiei
    else [x] ++ (fstwordaux xs) -- daca intalnim un caracter care nu e spatiu, atunci il afisam si continuam


-- functia fstword imbina cele doua functii de mai sus; astfel, avand un sir de caractere dat, mai intai elimina spatiile de la inceputul cuvantului, folosind spacetrim si apoi afiseaza primul cuvant, folosind fstwordaux;

fstword :: String -> String 
fstword [] = []
fstword s = fstwordaux (spacetrim s)
    

-- Cateva teste in ghci
-- *Main> fstword "abc cd"
-- "abc"
-- *Main> fstword "  Ana are mere"
-- "Ana"
-- *Main> fstword "Ana are mere"  
-- "Ana"
-- *Main> fstword " "           
-- ""
-- *Main> fstword " a a a"
-- "a"
-- *Main> fstword "       Ana are mere"
-- "Ana"



-- Exercitiul II

-- Scrieti o functie trimleft care primeste la input un sir de
-- caractere si scoate toate spatiile de la inceputul sirului de
-- intrare. Fara functii din biblioteca standard (exceptie:
-- if-then-else, ==)

-- Solutie
trimleft :: String -> String
trimleft [] = []
trimleft (x:xs) =
    if([x] == " ") then (trimleft xs)  -- "sarim" peste spatii pana cand
    else [x] ++ xs                      -- intalnimul caracter care nu este spatiu, moment in care afisam sirul incepand cu acel caracter 

-- Cateva teste in ghci
-- *Main> trimleft "     ana are mere"
-- "ana are mere"
-- *Main> trimleft "     a"           
-- "a"
-- *Main> trimleft "     " 
-- ""
-- *Main> trimleft "_  a   "
-- "_  a   "
-- *Main> trimleft "  a   " 
-- "a   "
-- *Main>



-- Exercitiul III

-- Scrieti o functie recursiva getfirst :: String -> (String, String)
-- care primeste la intrare un sir de caractere nevid si returneaza o
-- pereche formata din primul cuvant din acel sir si restul sirului;
-- Prin cuvant intelegem un sir de caractere fara spatii. Puteti
-- defini propriile functii ajutatoare, dar nu puteti utiliza functii
-- din biblioteca standard, cu exceptia celor de mai sus.

-- Solutie

-- functia care, avand un sir de caractere dat ca parametru, ne returneaza sirul lipsit de primul cuvant;
restofstringaux :: String -> String 
restofstringaux [] = []
restofstringaux (x:xs) = 
    if([x] == " ") then [x]++xs     -- in momentul in care intalnim un spatiu, afisam tot sirul incepand cu acesta
    else restofstringaux xs -- "sarim" peste caracterele care nu sunt spatii

-- ne folosim de trimleft pentru a ignora spatiile de la inceputul cuvantului;
restofstring :: String -> String 
restofstring [] = []
restofstring s = restofstringaux (trimleft s)

-- folosim funtia fstword definita mai sus pentru a obtine primul cuvant din sir si functia restofstring pentru a obtine restul sirului;
getfirst :: String -> (String, String)
getfirst [] = ([],[])
getfirst s = (fstword s, restofstring s)

-- Cateva teste in ghci
-- *Main>
-- *Main> getfirst "     ana are mere"
-- ("ana"," are mere")
-- *Main> getfirst " ana are mere"    
-- ("ana"," are mere")
-- *Main> getfirst "ana are mere" 
-- ("ana"," are mere")
-- *Main> getfirst "a"           
-- ("a","")
-- *Main> getfirst "a a"
-- ("a"," a")
-- *Main> getfirst "a " 
-- ("a"," ")
-- *Main> getfirst "a    "
-- ("a","    ")
-- *Main> getfirst " "    
-- ("","")
-- *Main> getfirst "" 
-- ("","")



-- Exercitiul IV
-- Se considera cunoscute functiile de mai jos:

lowerword :: String -> (String, String)
lowerword s = (map toLower s, "lower")

getwords :: String -> ([String], String)
getwords s = (words s, "words")

-- Observatie: Functiile words si toLower sunt din modulul Data.Char.

-- Cerinta: scrieti o functie `compose` de ordin superior (cat mai
-- generala) care poate fi folosita pentru a face compunerea
-- functiilor lowerword si getwords (aplicandu-le in aceasta ordine).

-- Un exemplu de rulare:
--- *Main> compose lowerword getwords "ASD ASDF"
--- (["asd","asdf"],"lower words")

-- Solutie
compose :: (a -> (a,String)) -> (a -> ([a],String)) -> a -> ([a],String)
compose f g x = (fst(g (fst(f x))), (snd (f x)) ++ " " ++ (snd (g x)))
-- pentru primul element din perechea ce va fi returnata:
    -- mai intai aplicam prima functie (f) pe argumentul x;
    -- apoi extragem doar primul element din pereche;
    -- apoi aplicam pe elementul extras a doua functie (g);
    -- apoi extragem din nou doar primul element din pereche/

-- pentru cel de-al doilea element din perechea ce va fi returnata:
    -- aplicam prima si a doua functie pe argumentul x;
    -- extragem pentru amandoua cel de-al doilea element din pereche;
    -- apoi elementele extrase le concatenam.

-- Cateva teste in ghci
-- *Main> compose lowerword getwords "ASD ASDF"
-- (["asd","asdf"],"lowerwords")
-- *Main> :r
-- [1 of 1] Compiling Main             ( Enia_Vlad-Ieftimie_E4.hs, interpreted )
-- Ok, one module loaded.
-- *Main> compose lowerword getwords "ASD ASDF"
-- (["asd","asdf"],"lower words")
-- *Main> compose lowerword getwords "   ANA ARE MERE"
-- (["ana","are","mere"],"lower words")
-- *Main> compose lowerword getwords "MeRe"           
-- (["mere"],"lower words")
-- *Main> compose lowerword getwords ""    
-- ([],"lower words")
-- *Main> compose lowerword getwords " "
-- ([],"lower words")



-- Exercitiul V

-- Scrieti o functie care numara elementele negative dintr-o lista de
-- numere intregi. In mod obligatoriu aceasta functie trebuie sa
-- foloseasca, in mod netrivial, una dintre functiile foldr sau foldl.

-- returnam (primul argument+1) daca al doilea argument este negativ sau doar primul argument daca al doilea este pozitiv;
isneg :: Integer -> Integer -> Integer
isneg a 0 = a
isneg a x = 
    if (x<0) then a+1 
    else a

countneg :: [Integer] -> Integer
countneg [] = 0
countneg list = foldl isneg 0 list

-- Solutie
-- COMPLETATI AICI COD HASKELL

-- Cateva teste in ghci
-- *Main> countneg [1,-1,-2,-3]
-- 3
-- *Main> countneg [1,-1,-2,-3]
-- 3
-- *Main> countneg [1,-1,2,-3] 
-- 2
-- *Main> countneg [1,1,2,3]  
-- 0
-- *Main> countneg [1,1,2,-13]
-- 1
-- *Main> countneg [1,1,2,-13]
-- *Main> countneg [0,0,0,1,-1]
-- 1
-- *Main> countneg [1]        
-- 0
-- *Main> countneg [-1]
-- 1
-- *Main> countneg [0,0,0,-0]
-- 0
-- *Main> countneg [0,0,0,1.-1]



-- Exercitiul VI

-- Proiectati un tip de date polimorfic, parametrizat cu tipul
-- valorile din noduri, care sa modeleze arbori binari (nu neaparat de
-- cautare, nu neaparat echilibrati) unde fiecare nod stocheaza si
-- adancimea la care se afla. Scrieti o functie care verifica pentru
-- un astfel de arbore daca adancimile sunt corecte (hint: foarte
-- probabil aveti nevoie de o functie auxiliara care sa primeasca si
-- nivelul curent).

-- Solutie
data Arb = Frunza | Nod Integer Integer Arb Arb deriving (Show, Eq)
-- exemplu arbore binar cu adancimi corecte: (Nod 8 0 (Nod 3 1 (Nod 1 2 Frunza Frunza) (Nod 6 2 Frunza Frunza )) (Nod 10 1 Frunza (Nod 14 2 Frunza Frunza)))

getnodelevel :: Arb -> Integer
getnodelevel (Nod val level st dr) = level

checklevelaux :: Arb -> Integer -> Bool
checklevelaux Frunza _ = True
checklevelaux (Nod val level st dr) x = (level == x) && (checklevelaux st (x+1)) && (checklevelaux dr (x+1))

checklevel :: Arb -> Bool 
checklevel Frunza = True
checklevel arbore = checklevelaux arbore 0

-- Cateva teste in ghci
-- *Main>
-- *Main> checklevel (Nod 8 0 (Nod 3 1 (Nod 1 2 Frunza Frunza) (Nod 6 2 Frunza Frunza )) (Nod 10 1 Frunza (Nod 14 2 Frunza Frunza)))
-- True
-- *Main> checklevel (Nod 8 0 (Nod 3 1 (Nod 1 2 Frunza Frunza) (Nod 6 2 Frunza Frunza )) (Nod 10 10 Frunza (Nod 14 2 Frunza Frunza)))
-- False
-- *Main> checklevel (Nod 8 0 (Nod 3 1 (Nod 1 2 Frunza Frunza) (Nod 6 2 Frunza Frunza )) (Nod 10 0 Frunza (Nod 14 2 Frunza Frunza))) 
-- False
-- *Main> checklevel (Nod 8 0 (Nod 3 1 (Nod 1 2 Frunza Frunza) (Nod 6 2 Frunza Frunza )) (Nod 10 1 Frunza (Nod 14 2 Frunza Frunza))) 
-- True
-- *Main> checklevel (Nod 8 1 (Nod 3 1 (Nod 1 2 Frunza Frunza) (Nod 6 2 Frunza Frunza )) (Nod 10 1 Frunza (Nod 14 2 Frunza Frunza)))
-- False
-- *Main>



-- Exercitiul VII

-- Proiectati un tip de date pentru reprezentarea expresiilor logice
-- care apar drept conditie pentru buclele while sau instructiunea if
-- in limbaje de nivel scazut, cum ar fi C sau Java. Puteti defini si
-- tipuri auxiliare (daca aveti nevoie). Nu este nevoie ca tipul de
-- date sa acopere *toate* posibilitatile: de exemplu, puteti ignora
-- apelurile de functii/metode. Incercati sa acoperiti o paleta larga
-- de expresii care apar in practica.

-- Solutie


-- Cateva teste in ghci (exemple de expresii)
-- *Main>
-- COMPLETATI AICI SUB FORMA DE COMENTARIU