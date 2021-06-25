import System.IO
import System.Environment
import Data.Char
import Control.Exception


--- Ex. 0.2 ---

-- main :: IO ()
-- main = (putStrLn "Hello, World!") >> (putStrLn "Hello") >> (putStrLn "World")



--- Ex. 0.3 ---

-- main :: IO ()
-- main = 
--     putStr "First name: " >>
--     hFlush stdout >>
--     (getLine >>=
--     (\firstName -> 
--         putStr "Last name: " >>
--         hFlush stdout >>
--         (getLine >>= 
--         (\lastName -> 
--             putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!") ))))



--- Ex. 0.4 ---

-- main :: IO ()
-- main = 
--     do
--     putStr "First name: "
--     hFlush stdout
--     firstName <- getLine  
--     putStr "Last name: "
--     lastName <- getLine
--     putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!")



--- Ex. 0.5 ---

-- main :: IO ()
-- main = 
--     do
--     putStrLn "What is your name?"
--     name <- getLine
--     putStrLn ("Hello, " ++ name ++ "!\n")
--     main

-- main :: IO ()
-- main = 
--     (putStrLn "What is your name?") >>
--     (getLine >>=
--     (\name -> putStrLn ("Hello, " ++ name ++ "!\n") >> main ))



--- Ex. 0.6 ---

-- main :: IO ()
-- main = 
--     putStr "First name: " >>
--     hFlush stdout >>
--     (getLine >>=
--     (\firstName -> 
--         putStr "Last name: " >>
--         hFlush stdout >>
--         (getLine >>= 
--         (\lastName -> 
--             putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!\n") >> main ))))


-- main :: IO ()
-- main = 
--     do
--     putStrLn "What is your name?"
--     name <- return "Victor"
--     putStrLn ("Hello, " ++ name ++ "!")

-- In acest program, comanda return "Victor" produce valoarea data ca argument, adica string-ul "Victor", iar aceasta valoare este denumita name mai departe in notatia do. Astfel, comanda putStrLn ("Hello, " ++ name ++ "!") ne va da sirul "Hello, Victor!".


-- main :: IO ()
-- main = 
--     do
--     putStrLn "What is your name?"
--     return ()
--     name <- getLine
--     putStrLn ("Hello, " ++ name ++ "!")

-- In acest program, comanda return () va produce valoarea data ca argument, adica o valoare de tip unit, fara insa a influenta in vreun fel rezultatul final, programul avand acelasi comportament ca un program lipsit de comanda return ().



--- Ex. 0.7 ---

-- main :: IO ()
-- main = 
--     do
--     putStr "First name: "
--     hFlush stdout
--     firstName <- getLine  
--     if firstName == "" then
--         return ()
--     else do
--         putStr "Last name: "
--         lastName <- getLine
--         if lastName == "" then
--             return ()
--         else do
--             putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!\n")
--             main




--- Ex. 0.8 ---

stringToUpper :: String -> String 
stringToUpper [] = []
stringToUpper (x:xs) = [toUpper x] ++ (stringToUpper xs)

-- main :: IO ()
-- main = 
--     do
--     putStr "Scrie string-ul: "
--     hFlush stdout
--     sir <- getLine 
--     if sir == "" then
--         return ()
--     else do 
--         putStrLn ("String-ul uppercase: " ++ stringToUpper sir ++ "\n")
--         main



--- Ex. 0.9 ---

    -- openFile :: FilePath -> IOMode -> IO Handle

    -- hGetContents :: Handle -> IO String

    -- hGetLine :: Handle -> IO String
    
    -- hClose :: Handle -> IO ()
    
    -- getArgs :: IO [String]
    
    -- getProgName :: IO String
    
    -- hPutStr :: Handle -> String -> IO ()
 

        
--- Ex. 0.10 ---

proceseaza :: Handle -> IO ()
proceseaza handle = 
    do
    b <- hIsEOF handle
    if b then
        return () -- fisier gol
    else do
        line <- hGetLine handle
        putStrLn line
        proceseaza handle

citireFisier :: IO ()
citireFisier =
    do
    putStrLn "\n"
    handle <- openFile "exemplu.txt" ReadMode
    proceseaza handle
    putStrLn "\n"
    hClose handle  



--- Ex. 0.11 ---

citireFisier' :: FilePath -> IO ()
citireFisier' numeFisier =
    do
    putStrLn "\n"
    handle <- openFile numeFisier ReadMode
    proceseaza handle
    putStrLn "\n"
    hClose handle  

-- main :: IO ()
-- main = 
--     do
--         xs <- getArgs
--         citireFisier' (xs !! 0)



--- Ex. 0.12 ---

tratare :: IOException -> IO ()
tratare ex =
    do
    program <- getProgName 
    putStrLn program

-- main :: IO ()
-- main = 
--     do
--     xs <- getArgs 
--     catch (citireFisier' (xs !! 0)) tratare



--- Ex. 0.13 ---

proceseaza' :: Handle -> IO ()
proceseaza' handle = 
    do
    b <- hIsEOF handle
    if b then
        return () -- fisier gol
    else do
        line <- hGetLine handle
        putStrLn (stringToUpper line)
        proceseaza' handle

citireFisier'' :: FilePath -> IO ()
citireFisier'' numeFisier =
    do
    putStrLn "\n"
    handle <- openFile numeFisier ReadMode
    proceseaza' handle
    putStrLn "\n"
    hClose handle  

-- main :: IO ()
-- main = 
--     do
--     xs <- getArgs 
--     catch (citireFisier'' (xs !! 0)) tratare



--- Ex. 0.14 ---

binSearch :: Integer -> Integer ->  IO () 
binSearch l r = 
    let m = (l + r) `div` 2 in  
        if (r<l) then putStrLn ("Atunci numarul este " ++ (show m))
        else 
            do
            putStrLn ("numarul de ghicit este >=" ++ (show m))
            rasp <- getLine
            if ((rasp /= "da") && (rasp /= "nu")) then 
                do
                putStrLn "Nu am inteles."
                binSearch l r
            else
                do
                if (rasp == "nu") then 
                    binSearch l (m-1)
                else 
                    binSearch (m+1) r
        
ghiceste :: IO ()
ghiceste = binSearch 1 100



--- Ex. 0.15 ---

-- Atunci cand se executa actiunea intoarsa de main, de exemplu, actiunile se evalueaza rand pe rand si astfel putem avea programe imperative.
-- Intr-un limbaj strict, in cazul actiunilor de citire, nu putem evalua ceea ce citim inainte de a face citirea propriu-zis. 


--- Ex. 0.16 ---

citireFisier3 :: IO ()
citireFisier3 =
    do
    handle <- openFile "exemplu.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

citireFisier4 :: IO ()
citireFisier4 =
    do
    handle <- openFile "exemplu.txt" ReadMode
    contents <- hGetContents handle
    hClose handle

-- main :: IO ()
-- main = citireFisier3
-- (0.09 secs, 1,239,712 bytes)

-- main :: IO ()
-- main = citireFisier4
-- (0.00 secs, 69,336 bytes)

-- Asadar, se poate obeserva ca in cazul in care afisam continutul, executia dureaza mai mult decat in cazul in care nu afisam continutul.
-- Mai mult decat atat, atunci cand nu afisam continutul, executia este aproape instantanee.
-- Asadar, am putea trage concluzia ca functia hGetContents nu citeste continutul unui fisier decat daca este necesar.