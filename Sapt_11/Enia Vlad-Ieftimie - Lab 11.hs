--- Fisa de exercitii 11 ---

type Id = String 

data Term = Var Id | App Term Term | Lambda Id Term deriving(Show, Eq)


--- Ex. 0.1 ---

-- Lambda "x" (Lambda "y" (Var "x"))


--- Ex. 0.2 ---

subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term
                        | True = Var id'
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') 
                                | id == id' = Lambda id' term'
                                | True = (Lambda id' (subst id term term')) 


-- Exemple
    -- 1. subst "x" (Var "y") (Var "x") = Var "y"
    -- 2. subst "y" (Var "z") (Var "x") = Var "x"
    -- 3. subst "y" (Var "z") (App (Var "x") (Var "y")) = App (Var "x") (Var "z")
    -- 4. subst "y" (Var "z") (App (Var "y") (Var "x")) = App (Var "z") (Var "x")
    -- 5. subst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x"))) = Lambda "x" (App (Var "y") (Var "x"))
    -- 6. subst "x" (Lambda "z" (Var "z")) (Lambda "y" (App (Var "y") (Var "x"))) = Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))

    -- 1. subst "y" (Var "x") (Lambda "x" (Var "y")) = Lambda "x" (Var "x")


--- Ex. 0.3 ---

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) 
                    | id == hd = remove id tl
                    | True = hd : (remove id tl)


-- Ex. 0.4 ---

free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = (free term1) ++ (free term2)
free (Lambda id term) = remove id (free term) 


--- Ex. 0.5 ---

vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = (vars term1) ++ (vars term2)
vars (Lambda id term) = id : (vars term) 


--- Ex. 0.6 ---
 
fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ (show index)) `elem` ids then fresh' ids (index + 1)
else "n" ++ (show index)

fresh :: [Id] -> Id
fresh ids = fresh' ids 0


--- Ex. 0.7 ---

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term
                            | True = (Var id')
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid 
                                        | id == id' = Lambda id' term'
                                        | id' `elem` (free term) = let id'' = fresh avoid in (Lambda id'' (casubst id term (subst id' (Var id'') term') avoid))
                                        | True = Lambda id' (casubst id term term' avoid)

                            
{-
    Exemple:
        -- 1. casubst "x" (Var "y") (Var "x") [] = Var "y"
        -- 2. casubst "y" (Var "z") (Var "x") [] = Var "x"
        -- 3. casubst "y" (Var "z") (App (Var "x") (Var "y")) [] = App (Var "x") (Var "z")
        -- 4. casubst "y" (Var "z") (App (Var "y") (Var "x")) [] = App (Var "z") (Var "x")
        -- 5. casubst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x"))) [] = Lambda "x" (App (Var "y") (Var "x"))
        -- 6. casubst "x" (Lambda "z" (Var "z")) (Lambda "y" (App (Var "y") (Var "x"))) [] = Lambda "y" (App (Var "y") (Lambda "z" (Var "z")))  
    

        -- 1. casubst "y" (Var "x") (Lambda "x" (Var "y")) (free (Var "x")) = Lambda "n0" (Var "x")
-}


