module Main where

import Lib
import Nat

import Convert

import System.IO

f :: Int -> Int -> Int 
f x y = convert ( add  (convert' x) (convert' y) )  -- Convertim x si y din Int in Nat, 

main :: IO ()
main = 
    do
        putStr "Primul nr: "
        hFlush stdout
        x <- getLine 

        putStr "Al doilea nr: "
        hFlush stdout
        y <- getLine 

        putStr "Rezultat: "
        hFlush stdout
        putStrLn (show (f (read x) (read y)))   -- Convertim x si y din [Char] in Int folosind read, aplicam functia f de mai sus, si apoi afisam rezultatul folosind Show

