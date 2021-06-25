module Mem where

type Mem = [(String, Integer)] -- de optimizat mai tarziu in Map

empty :: Mem
empty = []

select :: String -> Mem -> Maybe Integer
select x [] = Nothing
select x ((y, v):tl) = if x == y then Just v else select x tl

m1 = [("aaaa", 4), ("b", 42)]

update :: Mem -> String -> Integer -> Mem
update [] x v = [(x, v)]
update ((y, w):tl) x v = if x == y then ((x, v):tl) else (y, w):(update tl x v)
