---- Monada IO ----
-- Ce este IO? Este un tip parametrizat.

--main :: IO ()
--main = putStrLn "Hello, World!"

-- Valorile de tip "IO a" sunt actiuni pe care, daca sistemul le executa, la sfarsit produc o valoare de tip "a".

-- () reprezinta o valoare de tip unit.

-- Asa arata un program "Hello, World!" in Hasqell.

-- Pentru a execut: 
--   ghc notite_curs_7.hs -o curs7.exe
--   ./curs7.exe

-- Exemple in care secventiem actiuni folosind ">>"

-- main = (>>) (putStrLn "Hello, World!") (putStrLn "Hello, class!")

-- main =  (putStrLn "Hello, World!") >> (putStrLn "Hello, class!")

{-
main =  
    putStrLn "Hello, World!" >> 
    putStrLn "What is your name?" >>
    getLine >>
    putStrLn "Hello, ...!"
-}
-- Ne dorim ca la sfarsit sa ne afiseze "Hello, [string-ul citit de getLine]"
-- Pentru a realiza acest lucru, folosim ">>="

-- main =  
--     putStrLn "Hello, World!" >> 
--     putStrLn "What is your name?" >>
--     (getLine >>=
--     (\name -> (putStrLn ("Hello, " ++ name ++ "!"))))

-- Pentru a executa programul, putem in ghci, sa ii dam load, si apoi sa scriem "main"



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- In mod normal:
--  1. se evlueaza main
--  2. se executa actiunea intoarsa de main
-- De fapt, cand se executa actiunea intoarsa de main, abia atunci incep rand pe rand sa se evalueze actiunile. In acest mod este folosita evaluarea lenesa pentru a permite programe imperative. 

-- Exista niste "syntax sugar" care ne lasa sa scriem main ul de mai sus folosind notatia "do"


-- main = 
--     do
--     putStrLn "Hello!"
--     putStrLn "What is your name?"
--     getLine
--     putStrLn "Hello, ...!"
    
-- main = 
--     do
--     putStrLn "Hello!"
--     putStrLn "What is your name?"
--     name <- getLine
--     putStrLn ("Hello " ++ name ++ "!")

-- OBS! Aciunile trebuie sa fie aliniate cu "do".

-- OBS! a nu se confunda "x <- getLine" cu "let x = getLine"

-- In cazul "<-" se eexecuta getLine, iar valoarea produsa de getLine este denumita "x" mai departe in notatia do, deci x va fi de tip String

-- In cazul "let", numele "x" este o actiune, aceeasi cu getLine, deci x va fi de tip "IO String"



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- main = 
--     do
--     putStrLn "Hello!"
--     putStrLn "What is your name?"
--     name <- getLine
--     putStrLn ("Hello " ++ name ++ "!")
--     main

-- Aici am buclat probramul main, apeland main in ultima ramura do. Aceaasta recursei insa fa fi infinita.

-- main = 
--     do
--     putStrLn "Hello!"
--     putStrLn "What is your name?"
--     name <- getLine
--     if name == "" then
--         return ()
--     else 
--         do
--         putStrLn ("Hello " ++ name ++ "!")
--         main

-- OBS! A nu se confunda cu returnu din c sau java. "return x" intoarce o actiunea care nu efectueaza niciun efect secundar, si la sfarsit produce valoarea "x". return nu inseamna ca se intrerupe executia

-- OBS! getCHar si functii pure si imperative ~minutul 46

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- trebuie incarcat modului System.IO folosind :m + System.IO

import System.IO
import System.Environment
import Control.Exception
--import System.Random -- ATENTIE TREBUIE INSTALAT PACHETU

startsWith :: String -> String -> Bool 
startsWith [] _ = True 
startsWith _ [] = False 
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

isComment :: String -> Bool
isComment [] = False 
isComment (' ':xs) = isComment xs
isComment s = startsWith "--" s

proceseaza :: Handle -> IO ()
proceseaza handle = 
    do
    b <- hIsEOF handle
    if b then
        return () -- fisier gol
    else do
    line <- hGetLine handle
    --if line !! 0 == '-' ...  -- vrem sa ignoram liniile care sunt comentate. putem face verificare in aceasta functie care e in monada IO, dar o solutie mai buna ar fi sa cream o noua functie startsWith si isComment
    if isComment line then
        return ()
    else
        putStrLn line
    proceseaza handle

citireFisier :: IO ()
citireFisier =
    do
    handle <- openFile "notite_curs_7.hs" ReadMode
    proceseaza handle
    hClose handle          

-- citireFisier :: IO ()
-- citireFisier =
--     do
--     handle <- openFile "notite_curs_7.hs" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- main :: IO ()
-- main = citireFisier


---------------------------------------------------------------------------------------------------------------------------------------------------

-- Pt a citi un fisier dat ca argument

citireFisier' :: FilePath -> IO ()
citireFisier' numeFisier =
    do
    handle <- openFile numeFisier ReadMode
    proceseaza handle
    hClose handle    

-- main :: IO ()
-- main =
--     do
--     xs <- getArgs 
--     program <- getProgName 
--     print xs
--     print program
--     citireFisier' (xs !! 0)


---------------------------------------------------------------------------------------------------------------------------------------------------

-- Exceptii

tratare :: IOException -> IO ()
tratare ex = putStrLn "Atentie, ceva nu a mers bines!"

main :: IO ()
main =
    do
    xs <- getArgs 
    program <- getProgName 
    print xs
    print program
    catch (citireFisier' (xs !! 0)) tratare



---------------------------------------------------------------------------------------------------------------------------------------------------

-- Numere pseudo-aleatoare

