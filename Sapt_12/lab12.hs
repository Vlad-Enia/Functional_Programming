

type Id = String

data Term = Var Id | App Term Term | Lambda Id Term deriving (Show,Eq)

-- Exercitiul 0.2

subst :: Id -> Term -> Term -> Term
subst id t' (Var id')
 | id == id' = t'
 | otherwise = Var id'
subst id t' (App t1 t2) = App (subst id t' t1) (subst id t' t2)
subst id term (Lambda id' term')
 | id == id' = Lambda id' term'
 | otherwise = Lambda id' (subst id term term')


replaceid = "x"
tot' = Var "z"

oldt = Lambda "y" (Var "x")


-- Exercitiul 0.3

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (x:xs)
 | id == x = remove id xs
 | otherwise = x : remove id xs

lista = ["a","b","c","d"]
toremove = "x"


-- Exercitiul 0.4
free :: Term -> [Id]
free (Var id) = [id]
free (App t1 t2) = free t1 ++ free t2
free (Lambda id t) = remove id (free t)

termen2 = Lambda "x" (Lambda "y" (Lambda "x" (App (App (Var "x") (Var "y")) (Var "z"))))
termen3 = App (Var "x") (Lambda "x" (Var "x"))


-- Exercitiul 0.5
vars :: Term -> [Id]
vars (Var id) = [id]
vars (App t1 t2) = vars t1 ++ vars t2
vars (Lambda id t) = id : vars t


-- Exerctiul 0.6

fresh' :: [Id] -> Int -> Id
fresh' ids index =
    if "n" ++ (show index) `elem` ids then
        fresh' ids (index+1)
    else
        "n" ++ show index

listaids = ["x","y","z","y","n0"]

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

-- Exercitiul 0.7

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _
 | id == id' = term
 | otherwise = Var id'
casubst id term (App t1 t2) avoid = App (casubst id term t1 avoid) (casubst id term t2 avoid)
casubst id term (Lambda id' t') avoid
 | id == id' = Lambda id' t'
 | id' `elem` free term =
     let id'' = fresh avoid in
        Lambda id'' (casubst id term (subst id' (Var id'') t') avoid)
 | otherwise = Lambda id' (casubst id term t' avoid)

termen4 = Lambda "x" (Var "y")

replaced = "y"
replacer = Var "x"
expression = Lambda "x" (Var "y")

-- Exercitiul 0.8

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id t) t') avoid = Just (casubst id t' t avoid)
reduce1' (App t1 t2) avoid =
    case reduce1' t1 avoid of
        Nothing -> case reduce1' t2 avoid of
                        Nothing -> Nothing
                        Just t2' -> Just (App t1 t2')
        Just t1' -> Just (App t1' t2)
reduce1' (Lambda id t) avoid = case reduce1' t avoid of
                                    Nothing -> Nothing
                                    Just t' -> Just (Lambda id t')


reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)



-- Exercitiul 0.9
reduce :: Term -> Term
reduce t = maybe t reduce (reduce1 t)



-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Functii ajutatoare din laboratorul 11 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



--- Ex. 0.1. ---

x = Var "x"
y = Var "y"
z = Var "z"
term1 = Lambda "x" x
term2 = App term1 term1
term3 = Lambda "y" (Lambda "x" term2)
term4 = App term3 term1

t19 = Lambda "x" (Var "x")
t29 = Lambda "z" (Var "z")
t39 = App t29 y
t49 = App t19 t39

ex01 = reduce1 term1 -- Nothing
ex02 = reduce1 term2 -- Just (Lambda "x" (Var "x"))
ex03 = reduce1 term3 -- Just (Lambda "y" (Lambda "x" (Lambda "x" (Var "x"))))
ex04 = reduce1 term4 -- Just (Lambda "x" (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))))

ex09 = reduce t49 -- Var "y"
ex04' = reduce term4 -- Lambda "x" (Lambda "x" (Var "x"))



--- Ex. 0.2 ---

--- 1. ---                                                         --- Boolean ---

true :: Term
true = Lambda "x" (Lambda "y" x)

false :: Term
false = Lambda "x" (Lambda "y" y)

b :: Term
b = Var "b"

b1 :: Term
b1 = Var "b1"

b2 :: Term
b2 = Var "b2"

lambdaAnd :: Term
lambdaAnd   = Lambda "b1" (Lambda "b2" (App (App b1 b2) false))     --- AND ---

exAnd1 :: Term
exAnd1 = reduce (App (App lambdaAnd false) false)

exAnd2 :: Term
exAnd2 = reduce (App (App lambdaAnd false) true)

exAnd3 :: Term
exAnd3 = reduce (App (App lambdaAnd true) false)

exAnd4 :: Term
exAnd4 = reduce (App (App lambdaAnd true) true)


lambdaOr :: Term
lambdaOr = Lambda "b1" (Lambda "b2" (App(App b1 true) b2))          --- OR ---

exOr1 :: Term
exOr1 = reduce (App (App lambdaOr false) false)

exOr2 :: Term
exOr2 = reduce (App (App lambdaOr false) true)

exOr3 :: Term
exOr3 = reduce (App (App lambdaOr true) false)

exOr4 :: Term
exOr4 = reduce (App (App lambdaOr true) true)


lambdaNot :: Term
lambdaNot = Lambda "b" (App (App b false) true)                     --- NOT ---

exNot1 :: Term
exNot1 = reduce (App lambdaNot false)

exNot2 :: Term
exNot2 = reduce (App lambdaNot true)


--- 2. ---                                                          --- Nr. Naturale ---

s :: Term
s = Var "s"

n :: Term
n = Var "n"

m :: Term
m = Var "m"

zero = Lambda "s" (Lambda "z" z)                                                --- Zero ---

lambdaSucc = Lambda "n" (Lambda "s" (Lambda "z" (App s (App (App n s) z))))     --- Succ ---

exSucc1 = reduce (App lambdaSucc zero)

intToLambda :: Int -> Term                                                      --- int -> Lambda ---
intToLambda 0 = zero
intToLambda n = reduce (App lambdaSucc (intToLambda (n-1)))                     

-- exIntToLambda1 = reduce (intToLambda 1)
-- exIntToLambda2 = reduce (intToLambda 2)
-- exIntToLambda5 = reduce (intToLambda 5)

lambdaToInt :: Term -> Int                                                      --- Lambda -> int ---                          
-- lambdaToInt zero = 0                             -- va merge mereu pe ramura asta daca o decomentez
lambdaToInt (Lambda s term) = length (free term)    -- pentru ca imi doresc sa obtin numarul de aparitii a variabilei "s", insa am doar functia free la dispozitie, voi calcula free de ceea ce urmeaza dupa Lambda "s";


lambdaPlus = Lambda "n" (Lambda "m" (Lambda "s" (Lambda "z" (App (App n s) (App (App m s) z)))))    --- PLUS ---

exLambdaPlus = lambdaToInt (reduce (App(App lambdaPlus (intToLambda 120)) (intToLambda 250)))


lambdaMult = Lambda "n" (Lambda "m" (App (App n (App lambdaPlus m)) zero))      --- MULT ---
exLambdaMult = lambdaToInt (reduce (App(App lambdaMult (intToLambda 25)) (intToLambda 6)))


--- 3. ---                                                      --- Predicate ---

isZero = Lambda "n" (App (App n (Lambda "a" false)) true)       --- is zero ---

exIsZero0 = reduce (App isZero (intToLambda 0))

exIsZero1 = reduce (App isZero (intToLambda 25))



--- 4. ---

f = Var "f"

p = Var "p"

lambdaPair = Lambda "f" (Lambda "s" (Lambda "b" (App (App b f) s)))         --- Pair ---

lambdaFst = Lambda "p" (App p true)                                         --- First ---

lambdaSnd = Lambda "p" (App p false)                                        --- Second ---

exPair1 = reduce (App lambdaFst (App (App lambdaPair true) false))

exPair2 = reduce (App lambdaSnd (App (App lambdaPair true) false))


--- 3. ---

lambdaTrsf = Lambda "p" (App (App lambdaPair (App lambdaSnd p)) (App lambdaSucc (App lambdaSnd p)))

lambdaPred = Lambda "n" (App lambdaFst (App (App n lambdaTrsf) (App (App lambdaPair zero) zero)))

exPred = lambdaToInt (reduce (App lambdaPred (intToLambda 100)))

lambdaMinus = Lambda "n" (Lambda "m" (App (App m lambdaPred) n))                                         --- MINUS ---
-- n - m = aplicam "predecesor" de m ori asupra lui n

exLambdaMinus1 = lambdaToInt (reduce (App (App lambdaMinus (intToLambda 3)) (intToLambda 5)))

exLambdaMinus2 = lambdaToInt (reduce (App (App lambdaMinus (intToLambda 10)) (intToLambda 5)))

lambdaLeq = Lambda "n" (Lambda "m" (App isZero (App (App lambdaMinus n) m)))                                      --- <= ---
-- verificam daca diferenta dintre cele doua este zero

exLambdaLeq1 = reduce (App (App lambdaLeq (intToLambda 3)) (intToLambda 5))
exLambdaLeq2 = reduce (App (App lambdaLeq (intToLambda 5)) (intToLambda 3))
exLambdaLeq3 = reduce (App (App lambdaLeq (intToLambda 3)) (intToLambda 3))

lambdaEq = Lambda "n" (Lambda "m" (App (App lambdaAnd (App (App lambdaLeq n) m)) (App (App lambdaLeq m) n)))    --- == ---
-- verificam daca (n <= m) && (m <= n)

exLambdaEq1 = reduce (App (App lambdaEq (intToLambda 3)) (intToLambda 3))
exLambdaEq2 = reduce (App (App lambdaEq (intToLambda 5)) (intToLambda 6))

