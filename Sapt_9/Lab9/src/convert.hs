module Convert where

import Nat ( Nat(..) )

convert :: Nat -> Int
convert Zero = 0
convert (Succ x) = 1 + convert x -- adugam cate 1 si scadem din x cate un succ

convert' :: Int -> Nat
convert' 0 = Zero
convert' x = Succ (convert' (x-1)) -- aduaugam cate un succ, scazand din x cate 1