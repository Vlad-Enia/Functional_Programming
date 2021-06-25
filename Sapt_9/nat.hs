module Nat
(
    add
    , Nat(..) -- (..) = toti constructorii, sau putem mentiona explicit
) where

-- Numele fisierului in care este scris modulul = numele modulului
-- Numele modulului trebuie sa inceapa cu litera mare

data Nat = Zero | Succ Nat deriving Show

-- add :: Nat -> Nat -> Nat
-- add Zero x = x
-- add (Succ y) x = Succ (add y x)
-- Nu este tail recursive

-- Astfel, avem nevoie de o functie auxiliara, care tine doar de implementarea mea particulara si nu as dori ca utilizatorul bibliotecii Nat
-- sa foloseasca direct aceasta functie
addAux :: Nat -> Nat -> Nat 
-- addAux x y a = x + y + a
addAux Zero x = x
addAux (Succ y) x = addAux y (Succ x)


add:: Nat -> Nat -> Nat
add x y = addAux x y
-- Aceasta este functia la care ne dorim ca utilizatorul sa aiba acces.
-- In acest fel, adaugam la module explicit toti identificatorii pe care ii exportam, inclusiv tipurile de date


