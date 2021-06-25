module Par where

import Mem
import Lib
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

--                  stream-ul de intrare
--                        vvvvvv
type Parser = Parsec Void String
--                   ^^^^
--              erori de parsare

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifierStr :: Parser String
identifierStr = lexeme ((:) <$> letterChar <*> many alphaNumChar)

identifier :: Parser Exp
identifier = Var <$> identifierStr

integer :: Parser Exp
integer = Num <$> lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

factor :: Parser Exp
factor = identifier <|>
         integer <|>
         parens aexp

aexp :: Parser Exp
aexp = try ((\x _ y -> Plus x y) <$> factor <*> (symbol "+") <*> aexp) <|>
       try ((\x _ y -> Minus x y) <$> factor <*> (symbol "-") <*> aexp) <|>
       try ((\x _ y -> Mult x y) <$> factor <*> (symbol "*") <*> aexp) <|>
       try ((\x _ y -> Div x y) <$> factor <*> (symbol "/") <*> aexp) <|>
       factor


bfactor :: Parser Bexp 
bfactor = 
    try ((\x _ y -> Eq x y) <$> aexp <*> (symbol "=") <*> aexp) <|>
    try ((\x _ y -> Less x y) <$> aexp <*> (symbol "<") <*> aexp) <|>
    try ((\x _ y -> Greater x y) <$> aexp <*> (symbol ">") <*> aexp) <|>
        ((\x _ y -> Neq x y) <$> aexp <*> (symbol "!=") <*> aexp)

bexp :: Parser Bexp
bexp =    
  try ((\x _ y -> And x y) <$> bfactor <*> (symbol "&&") <*> bexp) <|>
  try ((\x _ y -> Or x y) <$> bfactor <*> (symbol "||") <*> bexp) <|>
  try ((\_ y -> Not y) <$> (symbol "!") <*> bexp) <|>
      bfactor
      

instr :: Parser Instr
instr = ((\_ x _ x1 _ x2 -> Ite x x1 x2) <$> (symbol "if") <*> bexp <*> (symbol "then") <*> block <*> (symbol "else") <*> block) <|>
        ((\_ x _ x1 -> While x x1) <$> (symbol "while") <*> bexp <*> (symbol "do") <*> block) <|>
        ((\_ x1 _ x2 _ x3 _ x4 -> For x1 x2 x3 x4) <$> (symbol "for") <*> aexp <*> (symbol ";") <*> bexp <*> (symbol ";") <*> instr  <*> (symbol "do") <*> block) <|>
        ((\x _ y _ -> Assign x y) <$> identifierStr <*> (symbol ":=") <*> aexp <*> (symbol ";"))
        
block :: Parser Instr
block = (\_ x _ -> Block x) <$> (symbol "{") <*> (many instr) <*> (symbol "}")

{-

data Exp = Num Integer | Var String | Plus Exp Exp | Minus Exp Exp deriving (Show, Eq)

data Bexp = Greater Exp Exp | Less Exp Exp | Eq Exp Exp | Neq Exp Exp deriving (Show, Eq)
data Instr = Assign String Exp | Block [Instr] | Ite Bexp Instr Instr | While Bexp Instr
             deriving (Show, Eq)
-}





{-
x = 13 * 13 * 7 * 7 * 5;          /* j1 */
y = 13 * 5 * 5;                   // j2
while x != y do
  if x > y then
    x := x - y                    // j3
  else
    y := y - x                    // j4
-}
