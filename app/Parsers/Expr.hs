module Parsers.Expr (Expr(..), expr) where

import GHC.Base ((<|>), many)
import Parsers.Base

-- Expr
data Expr = Literal Int | Unary UnaryOperator Expr | Binary BinaryOperator Expr Expr
data UnaryOperator = Pos | Neg
data BinaryOperator = Add | Sub | Mul | Div | Mod

instance Show Expr where
    show (Literal n) = show n
    show (Unary op a) = show op ++ show a
    show (Binary op a b) = show a ++ show op ++ show b

instance Show UnaryOperator where
    show Pos = "+"
    show Neg = "-"

instance Show BinaryOperator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"

-- Expr parsers
expr :: Parser Expr
expr = arithmetic

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
    <|> do symbol "("; x <- expr; symbol ")"; return (optimize x)
    <|> Literal <$> intLiteral

optimize :: Expr -> Expr
optimize (Unary op (Literal a)) = let f = unaryFunctionOf op in Literal $ f a
optimize (Binary op (Literal a) (Literal b)) = let f = binaryFunctionOf op in Literal $ f a b
optimize expr = expr

unaryFunctionOf :: Num a => UnaryOperator -> (a -> a)
unaryFunctionOf Pos = id
unaryFunctionOf Neg = \a -> -a

binaryFunctionOf :: Integral a => BinaryOperator -> (a -> a -> a)
binaryFunctionOf Add = (+)
binaryFunctionOf Sub = (-)
binaryFunctionOf Mul = (*)
binaryFunctionOf Div = div
binaryFunctionOf Mod = mod

-- Literal parsers
intLiteral :: Parser Int
intLiteral = token natural

doubleLiteral :: Parser Double
doubleLiteral = token real

charLiteral :: Parser Char
charLiteral = do char '\''; c <- item; char '\''; return c

stringLiteral :: Parser String
stringLiteral = do char '\"'; xs <- many $ itemWhere (/= '\"'); char '\"'; return xs