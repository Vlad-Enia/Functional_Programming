--- Enia Vlad Ieftimie 2E4 (2PF3) ---


--- 2.1 ---
    2  -- 2
    2 + 3 -- 5
    2 + 3 * 5 -- 17
    (2 + 3) * 5 -- 25
    3 / 5 -- 0.6
    45345345346536 * 54425523454534333 -- 2467944156711854340070394620488
    3 / 0  -- Infinity
    True  -- True
    False -- False
    True && False -- False
    True || False -- True 
    not True -- False
    2 <= 3 -- True
    not (2 <= 3) -- False
    (2 <= 3) || True -- True
    "aaa" == "aba" -- False
    "aba" == "aba" -- True
    "aaa" ++ "aba" -- "aaaaba"

--- 2.2 ---
    (+) 2 3 -- 5
    (+) 2 ((*) 3 5) -- 17
    (*) 5 ((+) 2 3) -- 25
    (/) 3 5  -- 0.6
    (*) 45345345346536 54425523454534333 -- 2467944156711854340070394620488
    (/) 3 0 -- Infinity
    (&&) True False -- False
    (||) True False -- True
    (<=) 2 3 -- True
    (||) True ((<=) 2 3) -- True
    (==) "aaa" "aba" -- False
    (==) "aba" "aba" -- True
    (++) "aaa" "aba" -- "aaaaba"


--- 2.3 ---
    :t True -- True :: Bool
    :t False -- False :: Bool
    :t True && False -- True && False :: Bool
    :t True && (2<=4) -- True && (2<=4) :: Bool

--- 2.4 --- 
    :t "aaa" -- "aaa" :: [Char] lista de caractere

--- 2.5 --- 
    :t 2 -- 2 :: Num p => p
    :t 2 + 3 -- 2 + 3 :: Num a => a
    :t (+) -- (+) :: Num a => a -> a -> a

--- 2.7 ---
    :t not -- not :: Bool -> Bool - tipul bool
    :t 2   -- 2 :: Num p => p - clasa de tipuri Num

--- 3.1 ---
    :t succ -- succ :: Enum a => a -> a
    succ 3  -- 4
    :t pred -- pred :: Enum a => a -> a
    pred 3  -- 2
    :t max -- max :: Ord a => a -> a -> a
    max 4 5 -- 5
    :t min -- min :: Ord a => a -> a -> a
    min 5 6 -- 5






