module Nat where

data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y) 

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ Zero) y = y
mult x (Succ Zero) = x
mult (Succ x) y = add y (mult x y) --adaugam y de succ si scadem pentru fiecare cate un succ din x
                                   -- sau adunam y de x ori 

myexp :: Nat -> Nat -> Nat
myexp Zero _ = Zero
myexp _ Zero = (Succ Zero)
myexp x (Succ Zero) = x
myexp x (Succ y) = mult x (myexp x y) -- inmultim x de y ori

comp :: Nat -> Nat -> Bool
comp Zero Zero = False 
comp Zero _ = True 
comp _ Zero = False
comp (Succ x) (Succ y) = comp x y -- le decrementam in acelasi ritm pe amandoua, pana ajungem pe un caz de baza
