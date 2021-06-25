
--- Ex. 0.1. ---

type Id = String 

data Type = TyBool | TyNat | Arrow Type Type deriving (Show,Eq)

data Term = Var Id | Lam Id Type Term | App Term Term | LamTrue | LamFalse | LamIte Term Term Term deriving (Show, Eq)


--- Ex. 0.2. ---

x = Var "x"

ex02 = App (Lam "x" TyBool x) LamTrue

data Context = Empty | Bind Id Type Context deriving (Show, Eq)

update :: Context -> Id -> Type -> Context
update gamma x ty = Bind x ty gamma


--- Ex. 0.3. ---

z = Var "z"
y = Var "y"

search :: Context -> Id -> Maybe Type
search Empty _ = Nothing 
search (Bind x ty gamma) y = 
    if x == y 
        then Just ty
    else
        search gamma y

gamma = Bind "x" TyBool (Bind "y" TyBool (Bind "z" TyNat Empty))
ex03 = search gamma "z"


--- Ex. 0.4. ---

typecheck :: Context -> Term -> Maybe Type
typecheck gamma (Var id) = search gamma id
typecheck gamma LamTrue = Just TyBool
typecheck gamma LamFalse = Just TyBool
typecheck gamma (LamIte t1 t2 t3) =
    case typecheck gamma t1 of
        Nothing -> Nothing
        Just TyBool ->  let tyt2 = typecheck gamma t2 in
                        let tyt3 = typecheck gamma t3 in
                        if tyt2 == tyt3 then
                            Just TyBool
                        else
                            Nothing
        Just _ -> Nothing
typecheck gamma (App t1 t2) =
    case typecheck gamma t1 of
        Just (Arrow ty1 ty2) -> 
            case typecheck gamma t2 of
                Just ty1 -> Just ty2
                _ -> Nothing
        _ -> Nothing
typecheck gamma (Lam x ty t) =
    case typecheck (update gamma x ty) t of
        Just ty' -> Just (Arrow ty ty')
        _ -> Nothing

t1 = App (Lam "x" TyBool (Var "x")) LamTrue
ex04 = typecheck Empty t1


--- Ex. 0.5. ---

t2 = Lam "x" (Arrow TyBool TyBool) (Lam "y" TyBool (App x y))
ex05 = typecheck Empty t2 -- Just (Arrow (Arrow TyBool TyBool) (Arrow TyBool TyBool))

t3 = Lam "x" TyBool (App x x)
ex05' = typecheck Empty t3 -- Nothing

t4 = Lam "x" (Arrow TyBool TyBool) (Lam "y" TyBool (App x z))
ex05'' = typecheck Empty t4 -- Nothing
ex05''' = typecheck (Bind "z" TyBool Empty) t4 --Just (Arrow (Arrow TyBool TyBool) (Arrow TyBool TyBool))
