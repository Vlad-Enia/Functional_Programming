--------- Lab 5 ---------

--- Ex. 1 ---

data Nat = Zero | Double Nat | DoubleAddOne Nat deriving Show


--- Ex. 2 ---

instance Eq Nat where
    (==) Zero Zero = True
    (==) Zero (Double Zero) = True 
    (==) Zero (DoubleAddOne _) = False
    (==) Zero (Double x) = Zero == x
    (==) (Double x) Zero = x == Zero
    (==) (DoubleAddOne _) Zero = False
    (==) (Double _) (DoubleAddOne _) = False
    (==) (DoubleAddOne _) (Double _) = False
    (==) (Double x) (Double y) = x == y
    (==) (DoubleAddOne y) (DoubleAddOne x) = x == y

--- 6 = Double (DoubleAddOne (DoubleAddOne Zero))
---   = Double (DoubleAddOne (DoubleAddOne (Double (Double (Double ... (Double Zero))))))


--- Ex. 3 ---

instance Ord Nat where
    (<=) Zero Zero = True
    (<=) Zero (Double Zero) = True 
    (<=) Zero (DoubleAddOne Zero) = True 
    (<=) Zero (Double x) = Zero <= x
    (<=) Zero (DoubleAddOne x) = True 
    (<=) (Double Zero) Zero = True 
    (<=) (Double Zero) (DoubleAddOne Zero) = True 
    (<=) (Double Zero) (Double x) = Zero <= x
    (<=) (Double Zero) (DoubleAddOne x) = True 
    (<=) (DoubleAddOne Zero) Zero = False
    (<=) (DoubleAddOne Zero) (Double Zero) = False
    (<=) (DoubleAddOne Zero) (DoubleAddOne Zero) = True
    (<=) (DoubleAddOne Zero) (Double x) = (DoubleAddOne Zero) <= x
    (<=) (DoubleAddOne Zero) (DoubleAddOne x) = True
    (<=) (Double x) Zero = False
    (<=) (Double x) (Double Zero) = False
    (<=) (Double x) (DoubleAddOne Zero) = x <= Zero
    (<=) (Double x) (Double y) = x <= y
    (<=) (DoubleAddOne x) Zero = False 
    (<=) (DoubleAddOne x) (Double Zero) = False
    (<=) (DoubleAddOne x) (DoubleAddOne Zero) = x <= Zero
    (<=) (DoubleAddOne x) (DoubleAddOne y) = x <= y
    (<=) (Double x) (DoubleAddOne y) = x <= y
    (<=) (DoubleAddOne x) (Double y) = x <= y

    -------------------------------------------------