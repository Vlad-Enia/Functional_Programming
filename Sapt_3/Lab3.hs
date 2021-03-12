-- 1.2 --
--data MobileDevice = Smartphone | Laptop | Tablet deriving (Show, Eq)
-- Smarthphone, Laptop si Tablet au tipul MobileDevice
{-
*Main> :t Tablet 
Tablet :: MobileDevice

*Main> :t Smartphone 
Smartphone :: MobileDevice

*Main> :t Laptop     
Laptop :: MobileDevice
-}

-- 1.3 --
data Culori = Red | Green | Blue deriving (Show, Eq)

data MobileDevice 
    = Smartphone Culori
    | Laptop Culori
    | Tablet Culori
    deriving (Show, Eq)


--- 1.4 --- 
afisCuloare :: MobileDevice -> Culori
afisCuloare (Smartphone culoare) = culoare
afisCuloare (Laptop culoare) = culoare
afisCuloare (Tablet culoare) = culoare


--- 2.1 --- 
data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)
-- Exemplu de arbore binar de cautare: (Nod 8 (Nod 3 (Nod 1 Frunza Frunza) (Nod 6 Frunza Frunza )) (Nod 10 Frunza (Nod 14 Frunza Frunza)))
-- Contraexemplu de arbore binar de cautare (Nod 2 (Nod 3 (Nod 1 Frunza Frunza) (Nod 6 Frunza Frunza )) (Nod 10 )

--- 2.2 --- 

nodVal :: Arb -> Integer 
nodVal (Nod val st dr) = val

isBST :: Arb -> Bool
isBST Frunza = True 
isBST (Nod val Frunza Frunza) = True
isBST (Nod val Frunza dr) = 
    if (val < (nodVal dr))
        then True 
    else False
isBST (Nod val st Frunza) = 
    if (val > (nodVal st))
        then True
    else False
isBST (Nod val st dr) = 
    if (val > (nodVal st) && val < (nodVal dr))
        then (isBST st) && (isBST dr)
    else False


--- 2.3 ---

search :: Arb -> Integer -> Bool 
search Frunza x = False 
search (Nod val st dr) x = 
    if ( val == x ) then True
    else if(x > val) then search dr x
    else search st x

--- 2.4 ---

isFrunza :: Arb -> Bool
isFrunza Frunza = True
isFrunza (Nod val st dr) = False


insertValue :: Arb -> Integer -> Arb 
insertValue Frunza x = Nod x Frunza Frunza
insertValue (Nod val st dr) x =
    if(x > val) then Nod val st (insertValue dr x) 
    else Nod val (insertValue st x) dr


--- 2.5 --- 
maxim :: Arb -> Maybe Integer
maxim Frunza = Nothing
maxim (Nod val _ Frunza) = Just val
maxim (Nod val st dr) = maxim dr


minim :: Arb -> Maybe Integer
minim Frunza = Nothing
minim (Nod val Frunza _) =  Just val
minim (Nod val st dr) = minim st

--- 2.6 --- 
--(Nod 8 (Nod 3 (Nod 1 Frunza Frunza) (Nod 6 Frunza Frunza )) (Nod 10 (Nod 9 Frunza Frunza) Frunza))
--(Nod 8 (Nod 3 (Nod 1 Frunza Frunza) (Nod 6 Frunza Frunza )) Frunza)
removeMax :: Arb -> Arb
removeMax (Nod val Frunza Frunza) = Frunza
removeMax (Nod val st Frunza) = st
removeMax (Nod val st dr) = Nod val st (removeMax dr)

--- 2.8 -- 
preOrder :: Arb -> [Integer]
preOrder Frunza = []
preOrder (Nod val Frunza Frunza) = [val]
preOrder (Nod val st Frunza) = val : (preOrder st)
preOrder (Nod val Frunza dr) = val : (preOrder dr)
preOrder (Nod val st dr) = val : (preOrder st ++ preOrder dr)

postOrder :: Arb -> [Integer]
postOrder Frunza = []
postOrder (Nod val Frunza Frunza) = [val]
postOrder (Nod val st Frunza) = (postOrder st) ++ [val]
postOrder (Nod val Frunza dr) = (postOrder dr) ++ [val]
postOrder (Nod val st dr) = (postOrder st ++ postOrder dr) ++ [val]

inOrder :: Arb -> [Integer]
inOrder Frunza = []
inOrder (Nod val Frunza Frunza) = [val]
inOrder (Nod val st Frunza) = (inOrder st) ++ [val]
inOrder (Nod val Frunza dr) = val : (inOrder dr)
inOrder (Nod val st dr) = (inOrder st ++ [val]) ++ inOrder dr

-- 3.1 --
height :: Arb -> Integer
height Frunza = 0
height (Nod _ Frunza Frunza) = 1
height (Nod _ st dr) = (max (height st) (height dr)) + 1

-- 3.2 --
-- (Nod 8 (Nod 6 Frunza (Nod 7 Frunza Frunza)) (Nod 9 Frunza (Nod 10 Frunza Frunza)))
isAVL :: Arb -> Bool
isAVL Frunza = True
isAVL (Nod _ Frunza Frunza) = True
isAVL (Nod val st dr) = (isBST (Nod val st dr)) && (abs (height st - height dr) <= 1) && (isAVL st) && (isAVL dr) 

--- 4.1 --- 

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

convert :: Nat -> Int
convert Zero = 0
convert (Succ x) = 1 + convert x -- adugam cate 1 si scadem din x cate un succ

convert' :: Int -> Nat
convert' 0 = Zero
convert' x = Succ (convert' (x-1)) -- aduaugam cate un succ, scazand din x cate 1

dif :: Nat -> Nat -> Nat
dif x Zero = x
dif Zero _ = Zero
dif (Succ x) (Succ y) = dif x y --decrementam x si y in acelasi ritm pana ajungem pe unul dintre cazurile de baza

myeq :: Nat -> Nat -> Bool 
myeq Zero Zero = True
myeq Zero _ = False
myeq _ Zero = False
myeq (Succ x) (Succ y) = myeq x y


mydiv :: Nat -> Nat -> Nat
mydiv Zero _ = Zero
mydiv _ Zero = Zero
mydiv x y = 
    if myeq x y then (Succ Zero)
    else Succ (mydiv (dif x y) y) -- adaugam cate un succ pentru fiecare scadere dintre x si y posibila
                                     -- rezultatul impartirii e practic = de cate ori putem scadea pe y din x


rest :: Nat -> Nat -> Nat
rest _ (Succ Zero) = Zero
rest x y
  | (comp x y) = x
  | (myeq x y) = Zero
  | otherwise = rest (dif x y) y  -- functia mymod din  lab2.hs