module Parsers.Program where

import GHC.Base ((<|>), some, many)
import Data.Char (isAlphaNum, isAscii, isAsciiUpper, isAsciiLower) 
import Parsers.Base (Parser, symbol, token, itemWhere)
import Parsers.Expr (Expr(..), expr)

type Block = [Statement]
type Identifier = String
data VarType = IntType | DoubleType | CharType

newtype Program = Program Block
data Statement = Declare VarType Identifier | Define VarType Identifier Expr | Assign Identifier Expr

instance Show Program where
    show (Program b) = show b

instance Show Statement where
    show (Declare t id) = show t ++ " " ++ id
    show (Define t id e) = show t ++ " " ++ id ++ " = " ++ show e
    show (Assign id e) = id ++ " = " ++ show e

instance Show VarType where
    show IntType = "Int"
    show DoubleType = "Double"
    show CharType = "Char"

program :: Parser Program
program = do Program <$> block

block :: Parser Block
block = do
    symbol "end"
    return []
    <|> do s <- statement; symbol ";"; ss <- block; return (s:ss)

statement :: Parser Statement
statement =
    do
        t <- varType
        id <- identifier
        do
            symbol "="
            Define t id <$> expr
            <|> return (Declare t id)
    <|>
    do
        id <- identifier
        symbol "="
        Assign id <$> expr

varType :: Parser VarType
varType = do
    symbol "Int"; return IntType
    <|> do symbol "Double"; return DoubleType
    <|> do symbol "Char"; return CharType

identifier :: Parser Identifier
identifier = do token identifier'
    where
        identifier' = do
            xs <- many $ itemWhere (== '_')
            y <- itemWhere isAsciiAlpha
            zs <- many $ itemWhere (\c -> isAsciiAlphaNum c || c == '_')
            return (xs ++ [y] ++ zs)

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c