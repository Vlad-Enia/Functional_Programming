module Lib where

import Mem

data Exp = Num Integer | Var String | Plus Exp Exp | Minus Exp Exp  | Div Exp Exp | Mult Exp Exp deriving (Show, Eq)

e1 = Plus (Num 123) (Num 7)
e2 = Plus (Num 42) (Var "aaaa")

type Error = String

eval :: Exp -> Mem -> Either Error Integer 
eval (Num x) _ = Right x                                      
eval (Plus e1 e2) mem = let r1 = eval e1 mem in
                        let r2 = eval e2 mem in
                          case r1 of                          -- verificam r1 (rezultatul evaluarii primei expresii)
                          Left str -> Left str                -- daca e de tip Left si un String str, atunci e caz de eroare si afisam str mai departe
                          Right vr1 ->                        -- daca e de tip Right si un Integer vr1, atunci mergem mai departe 
                            case r2 of                        -- si verificam r2 (rezultatul evaluarii celei de-a doua expresii)
                            Left str -> Left str              -- daca e de tip Left si un String str, atunci e caz de eroare si afisam str mai departe
                            Right vr2 -> Right (vr1 + vr2)    --  daca e de tip Right si un Integer vr2, atunci returnam Right si rezultatul operatiei
eval (Minus e1 e2) mem = let r1 = eval e1 mem in
                         let r2 = eval e2 mem in
                          case r1 of
                          Left str -> Left str
                          Right vr1 -> 
                            case r2 of
                            Left str -> Left str
                            Right vr2 -> Right (vr1 - vr2)
eval (Mult e1 e2) mem = let r1 = eval e1 mem in
                        let r2 = eval e2 mem in
                          case r1 of
                          Left str -> Left str
                          Right vr1 -> 
                            case r2 of
                            Left str -> Left str
                            Right vr2 -> Right (vr1 * vr2)
eval (Div e1 e2) mem = let r1 = eval e1 mem in
                       let r2 = eval e2 mem in
                        case r1 of
                        Left str -> Left str
                        Right vr1 -> 
                          case r2 of
                          Left str -> Left str
                          Right 0 -> Left "Error: Division by zero"   -- pe langa verificarile de mai sus, verificam mai intai daca impartitorul este 0, caz in care returnam Left si String-ul "Error: Division by zero."
                          Right vr2 -> Right (vr1 `div` vr2)

eval (Var x) mem = case select x mem of                                 -- daca cautand prin memoria mem 
                 Nothing -> Left "Error: variable not declared."      -- nu gasim ce cautam, atunci returnam Left si String-ul "Error: variable not declared."
                 Just v -> Right v                                      -- altfel returnam Right si valoarea gasita

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Bexp = Greater Exp Exp | Less Exp Exp | Eq Exp Exp | Neq Exp Exp | And Bexp Bexp | Or Bexp Bexp | Not Bexp deriving (Show, Eq)

be1 = Less (Var "aaaa") (Var "b")

beval :: Bexp -> Mem -> Either Error Bool
beval (Greater e1 e2) mem = let r1 = eval e1 mem in         
                            let r2 = eval e2 mem in             -- Evaluam ambele expresii
                              case r1 of
                              Left str -> Left str              -- Daca evaluarea primei expresii a rezultat in eroare, atunci o returnam
                              Right vr1 ->                      -- Altfel, preluam rezultatul evaluarii primei expresii, vr1; apoi vom vom verifica rezultatul evaluarii celei de-a doua expresii
                                case r2 of
                                Left str -> Left str            -- Daca evaluarea celei de-a doua expresii a rezultat in eroare, atunci o returnam
                                Right vr2 ->Right (vr1 > vr2)   -- Altfel, preluam rezultatul evaluarii celei de-a doua expresii, vr2; apoi evaulam expresia booleana compusa din vr1 si vr2 si ii returnam rezultatul;
                              
beval (Less e1 e2) mem = let r1 = eval e1 mem in
                         let r2 = eval e2 mem in
                            case r1 of
                            Left str -> Left str
                            Right vr1 -> 
                              case r2 of
                              Left str -> Left str
                              Right vr2 ->Right (vr1 < vr2)
beval (Eq e1 e2) mem = let r1 = eval e1 mem in
                       let r2 = eval e2 mem in
                            case r1 of
                            Left str -> Left str
                            Right vr1 -> 
                              case r2 of
                              Left str -> Left str
                              Right vr2 ->Right (vr1 < vr2)
beval (Neq e1 e2) mem = let r1 = eval e1 mem in
                        let r2 = eval e2 mem in
                          case r1 of
                          Left str -> Left str
                          Right vr1 -> 
                            case r2 of
                            Left str -> Left str
                            Right vr2 ->Right (vr1 /= vr2)
beval (And b1 b2) mem = let r1 = beval b1 mem in
                        let r2 = beval b2 mem in              -- Evaluam cele doua expresii booleene
                          case r1 of
                          Left str -> Left str                -- Daca prima a rezultat in eroare, o returnam
                          Right vr1 ->                        -- Altfel ii preluam rezultatul, vr1; apoi mergem mai departe
                            case r2 of
                            Left str -> Left str              -- Daca a doua a rezultat in eroare, o returnam
                            Right vr2 -> Right (vr1 && vr2)   -- Altfel ii preluam rezultatul, vr2; apoi evaulam expresia booleana compusa din vr1 si vr2 si ii returnam rezultatul;
beval (Or b1 b2) mem =  let r1 = beval b1 mem in
                        let r2 = beval b2 mem in
                          case r1 of
                          Left str -> Left str
                          Right vr1 -> 
                            case r2 of
                            Left str -> Left str
                            Right vr2 -> Right (vr1 || vr2)
beval (Not b) mem =  let r = beval b mem in                   -- Evaluam expresia booleana
                      case r of
                      Left str -> Left str                    -- Daca a rezultat in eroare, o returnam
                      Right vr -> Right (vr == False)         -- Altfel o negam;

                      

data Instr = Assign String Exp | Block [Instr] | Ite Bexp Instr Instr | While Bexp Instr | For Exp Bexp Instr Instr
             deriving (Show, Eq)

execute :: Instr -> Mem -> Either Error Mem
execute (Assign x e) mem =  let result = (eval e mem) in          -- evaluam mai intai expresia
                          case result of                       
                          Left str -> Left str                        -- daca evaluarea expresiei rezulta in Left si un String str, atunci memoria ramane neschimbata
                          Right result   -> Right ( update mem x result)   -- daca evaluarea expresiei rezulta in Right si un Integer result, atunci facem update x result
execute (Block []) mem = Right mem
execute (Block (hd:tl)) mem = let result = execute hd mem in
                              case result of
                              Left str -> Left str
                              Right mem' -> execute (Block tl) mem'
execute (Ite be ithen ielse) mem =  let rbe = beval be mem in         -- Evaluam expresia booleana
                                      case rbe of
                                      Left str -> Left str            -- Daca a rezultat in eroare, atunci o returnam
                                      Right vrbe ->                   -- Altfel, mergem mai departe, preluand rezultatul evaluarii expresiei booleene
                                          if vrbe then                -- Verificam rezultatul evaluarii expresiei booleene
                                            execute ithen mem
                                          else
                                            execute ielse mem
execute (While be i) mem =  let rbe = beval be mem in                 -- Evaluam expresia booleana
                              case rbe of
                              Left str -> Left str                    -- Daca a rezultat in eroare, atunci o returnam
                              Right vrbe ->                           -- Altfel, mergem mai departe, preluand rezultatul evaluarii expresiei booleene
                                if vrbe then                          -- Verificam rezultatul evaluarii expresiei booleene
                                let result = execute i mem in                              
                                  case result of
                                  Left str -> Left str
                                  Right mem' -> execute (While be i) mem'
                              else
                                Right mem
execute (For i1 be i instr) mem = let ri1 = eval i1 mem in                                              -- Evaluam prima expresie
                                    case ri1 of                                                         
                                    Left str -> Left str                                                -- Daca a rezultat in eroare, atunci o returnam
                                    Right vri1 ->                                                       -- Alfel mergem mai departe
                                      let rbe = beval be mem in                                         -- Evaluam expresia booleana
                                        case rbe of
                                        Left str -> Left str                                            -- Daca a rezultat in eroare, atunci o returnam
                                        Right vrbe ->                                                   -- Altfel, mergem mai departe, preluand rezultatul evaluarii expresiei booleene
                                          if vrbe then                                                  -- Verificam rezultatul evaluarii expresiei booleene
                                            let result = execute i mem in                               -- Daca e True, atunci executam instructiunea for-ului
                                              case result of
                                              Left str -> Left str                                      -- Daca a rezultat in eroare, atunci o returnam
                                              Right mem' ->                                             -- Atlfel mergem mai departe, actualizand memoria
                                                let rinstr = execute instr mem' in                      -- Executam instructiunea/blocul de instructiuni din corpul buclei
                                                  case rinstr of
                                                  Left str -> Left str                                  -- Daca a rezultat in eroare, atunci o returnam
                                                  Right mem'' -> execute (For i1 be i instr) mem''      -- Altfel actualizam memoria si mergem mai departe la urmatorul pas al buclei
                                          else
                                            Right mem                                                   -- Daca evaluarea expresiei booleene a rezultat in False, atunci returnam memoria neactualizata

                                                                                                                
i1 = Assign "aaaa" (Plus (Num 1) (Num 2))
i2 = Assign "b" (Plus (Var "aaaa") (Num 8))
i3 = Ite be1 (Assign "x" (Num 10)) (Assign "x" (Num 12))
i4 = While (Neq (Var "x") (Num 100)) (Assign "x" (Plus (Var "x") (Num 13)))
i5 = While (Less (Var "x") (Num 100)) (Assign "x" (Plus (Var "x") (Num 13)))
p1 = Block [i1, i2]
p2 = Block [i1, i2, i3]
p3 = Block [i1, i2, i3, i5]

{-
x = 13 * 13 * 7 * 7 * 5;          j1
y = 13 * 5 * 5;                   j2
while x != y do
  if x > y then
    x := x - y                    j3
  else
    y := y - x                    j4
-}

j1 = Assign "x" (Num (13 * 13 * 7 * 7 * 5))
j2 = Assign "y" (Num (13 * 5 * 5))
j3 = Assign "x" (Minus (Var "x") (Var "y"))
j4 = Assign "y" (Minus (Var "y") (Var "x"))
jite = Ite (Greater (Var "x") (Var "y")) j3 j4
jwh = While (Neq (Var "x") (Var "y")) jite
euclid = Block [j1, j2, jwh]


