module Parsers.Expr (Expr(..), expr) where

import GHC.Base ((<|>), many)
import Parsers.Base (Parser, Identifier, char, item, itemWhere, natural, real, symbol, token, identifier, separated)
import GHC.Num (andInteger)

-- Expr
data Expr = Literal Int | Identifier Identifier | Call Identifier [Expr] | Unary UnaryOperator Expr | Binary BinaryOperator Expr Expr
data UnaryOperator = Pos | Neg | Not
data BinaryOperator =
    And | Or |
    Eq | Ne |
    Lt | Le | Gt | Ge |
    Add | Sub |
    Mul | Div | Mod

instance Show Expr where
    show (Literal n) = show n
    show (Identifier id) = id
    show (Call id es) = id ++ "(" ++ show es ++ ")"
    show (Unary op a) = show op ++ show a
    show (Binary op a b) = show a ++ show op ++ show b

instance Show UnaryOperator where
    show Pos = "+"
    show Neg = "-"
    show Not = "not"

instance Show BinaryOperator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Lt = "<"
    show Le = "<="
    show Gt = ">"
    show Ge = ">="
    show Eq = "=="
    show Ne = "!="
    show And = "and"
    show Or = "or"

-- Expr parsers
expr :: Parser Expr
expr = expr' id
    where
        expr' f = do
            x <- conditional
            do
                symbol "and"; expr' (optimize . Binary And (f x))
                <|> do symbol "or"; expr' (optimize . Binary Or (f x))
                <|> return (optimize (f x))

conditional :: Parser Expr
conditional = conditional' id
    where
        conditional' f = do
            x <- relational
            do
                symbol "=="; conditional' (optimize . Binary Eq (f x))
                <|> do symbol "!="; conditional' (optimize . Binary Ne (f x))
                <|> return (optimize (f x))

relational :: Parser Expr
relational = relational' id
    where
        relational' f = do
            x <- arithmetic
            do
                symbol "<"; relational' (optimize . Binary Lt (f x))
                <|> do symbol "<="; relational' (optimize . Binary Le (f x))
                <|> do symbol ">"; relational' (optimize . Binary Gt (f x))
                <|> do symbol ">="; relational' (optimize . Binary Ge (f x))
                <|> return (optimize (f x))

arithmetic :: Parser Expr
arithmetic = arithmetic' id
    where
        arithmetic' f = do
            x <- term
            do
                symbol "+"; arithmetic' (optimize . Binary Add (f x))
                <|> do symbol "-"; arithmetic' (optimize . Binary Sub (f x))
                <|> return (optimize (f x))

term :: Parser Expr
term = term' id
    where
        term' f = do
            x <- factor
            do
                symbol "*"; term' (optimize . Binary Mul (f x))
                <|> do symbol "/"; term' (optimize . Binary Div (f x))
                <|> do symbol "%"; term' (optimize . Binary Mod (f x))
                <|> return (optimize (f x))

factor :: Parser Expr
factor = do
    symbol "+"; Unary Pos <$> factor
    <|> do symbol "-"; x <- Unary Neg <$> factor; return (optimize x)
    <|> do symbol "not"; x <- Unary Not <$> factor; return (optimize x)
    <|> do symbol "("; x <- expr; symbol ")"; return (optimize x)
    <|> Literal <$> intLiteral
    <|> do id <- identifier; symbol "("; es <- separated expr ","; symbol ")"; return (Call id es)
    <|> Identifier <$> identifier

optimize :: Expr -> Expr
optimize (Unary op (Literal a)) = let f = unaryFunctionOf op in Literal $ f a
optimize (Binary op (Literal a) (Literal b)) = let f = binaryFunctionOf op in Literal $ f a b
optimize expr = expr

unaryFunctionOf ::(Eq a, Num a) => UnaryOperator -> (a -> a)
unaryFunctionOf Pos = id
unaryFunctionOf Neg = \a -> -a
unaryFunctionOf Not = \a -> if a /= 0 then 0 else 1

binaryFunctionOf :: (Eq a, Integral a) => BinaryOperator -> (a -> a -> a)
binaryFunctionOf Add = (+)
binaryFunctionOf Sub = (-)
binaryFunctionOf Mul = (*)
binaryFunctionOf Div = div
binaryFunctionOf Mod = mod
binaryFunctionOf And = \a b -> if (a /= 0) && (b /= 0) then 1 else 0
binaryFunctionOf Or = \a b -> if (a /= 0) || (b /= 0) then 1 else 0
binaryFunctionOf Lt = \a b -> if a < b then 1 else 0
binaryFunctionOf Le = \a b -> if a <= b then 1 else 0
binaryFunctionOf Gt = \a b -> if a > b then 1 else 0
binaryFunctionOf Ge = \a b -> if a >= b then 1 else 0
binaryFunctionOf Eq = \a b -> if a == b then 1 else 0
binaryFunctionOf Ne = \a b -> if a /= b then 1 else 0

-- Literal parsers
intLiteral :: Parser Int
intLiteral = token natural

doubleLiteral :: Parser Double
doubleLiteral = token real

charLiteral :: Parser Char
charLiteral = do char '\''; c <- item; char '\''; return c

stringLiteral :: Parser String
stringLiteral = do char '\"'; xs <- many $ itemWhere (/= '\"'); char '\"'; return xs