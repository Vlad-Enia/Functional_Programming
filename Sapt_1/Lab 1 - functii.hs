--- Exercitii Lab 1: Enia Vlad Ieftimie 2E4 (2PF3) ---

--- 3.4 ---
    prodThree x y z = x * y * z


--- 3.5 --- 
    id x = x 

    sumThree x y z = x + y + z

--- 3.7 --- 
    -- sumThree :: Num a => a -> a -> a -> a

    sumThree :: Int -> Int -> Int -> Int

--- 3.8 ---
    {- 
        Acum, dupa ce am specificat explicit tipul functiei, tipul afisat este "sumThree :: Int -> Int -> Int -> Int",
        iar un apelul cu argumentele 3.2, 2 si 4 da urmatoarea eroare deoarece incercam sa dam un perametru de tip diferit de Int;

        <interactive>:25:10: error:
        * No instance for (Fractional Int) arising from the literal `3.2'
        * In the first argument of `sumThree', namely `3.2'
        In the expression: sumThree 3.2 2 4
        In an equation for `it': it = sumThree 3.2 2 4 
    -}


    sumThree' :: Double -> Double -> Double -> Double
    sumThree' x y z = x + y + z

    {-
        Pentru functia sumThree', functioneaza si apel cu parametri intregi, rezultatul fiind de forma x.0;
    -}

    myMax :: Int -> Int -> Int
    myMax x y = if x <= y then y else x

--- 3.9 --- 
    -- myMax :: Int -> Int -> Int

--- 3.10 --- 
    threeMax :: Int -> Int -> Int -> Int 
    threeMax x y z = myMax (myMax x y) z

--- 3.11 ---

    mySum :: Int -> Int
    mySum x = if x <= 0 then 0 else x + mySum (x - 1)

--- 3.12 --- 
    fib :: Int -> Int 
    fib 0 = 0
    fib 1 = 1
    --fib 2 = 1
    fib x = (fib (x-1)) + (fib (x-2))
    -- fib x = (fib x-1) + (fib x-2) => Exception: stack overflow...


--- 3.13 --- 
    cmmdc :: Int -> Int -> Int 
    cmmdc a b = if a==b 
                    then a 
                else 
                    if a>b then cmmdc (a-b) b
                    else cmmdc a (b-a)
    {- 
        Cand am dat enter dupa ce am scris signatura functiei si am apasat 
        enter mi-a pus automat un "-> Int" in plus si de asta primeam eroare.
    -}