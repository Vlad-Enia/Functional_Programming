module Main where

import Text.Megaparsec
import System.IO
import Lib
import Par

main :: IO ()
main = do
               handle <- openFile "p1.txt" ReadMode
               str <- hGetContents handle
               let (Right program) = runParser block "p1.txt" str in
                 print $ execute program empty
               hClose handle
