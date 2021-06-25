import Nat ( Nat(..) )
import Convert

import Test.QuickCheck

convert_prop :: Int -> Bool
convert_prop n = (n <= 0) || convert (convert' n) == n


main :: IO ()
main =
    do
        quickCheck convert_prop
        putStrLn "Done"
