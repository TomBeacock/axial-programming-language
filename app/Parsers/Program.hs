module Parsers.Program where

import GHC.Base ((<|>), some, many)
import Parsers.Base (Parser, Identifier, symbol, token, itemWhere, separated, identifier)
import Parsers.Expr (Expr(..), expr)

type Parameters = [Parameter]
type Block = [Statement]
data VarType = IntType | DoubleType | CharType

newtype Program = Program [Function]
data Function =
    Procedure Identifier Parameters Block
    | Function Identifier Parameters VarType Block
data Parameter = Parameter Identifier VarType
data Statement =
    Declaration Declaration
    | Assign Identifier Expr
    | If Expr Block | IfElse Expr Block Block
    | While Expr Block
    | Return Expr
data Declaration =
    Declare Identifier VarType
    | DefineInfer Identifier Expr
    | Define Identifier VarType Expr

instance Show Program where
    show (Program b) = show b

instance Show Function where
    show (Procedure id ps b) = "func " ++ id ++ " (" ++ show ps ++ ")" ++ " = " ++ show b ++ " end"
    show (Function id ps t b) = "func " ++ id ++ " (" ++ show ps ++ ")" ++ " -> " ++ show t ++ " = " ++ show b ++ " end"

instance Show Parameter where
    show (Parameter id t) = id ++ " " ++ show t

instance Show Statement where
    show (Declaration d) = show d
    show (Assign id e) = id ++ " = " ++ show e ++ ";"
    show (If e b) = "if " ++ show e ++ " then " ++ show b ++ " end"
    show (IfElse e b b') = "if " ++ show e ++ " then " ++ show b ++ "else" ++ show b' ++ " end"
    show (While e b) = "while " ++ show e ++ " do " ++ show b ++ " end"
    show (Return e) = "return " ++ show e ++ ";"

instance Show Declaration where
    show (Declare id t) = "var " ++ id ++ " " ++ show t ++ ";"
    show (DefineInfer id e) = "var " ++ id ++ " = " ++ show e ++ ";"
    show (Define id t e) = "var " ++ id ++ " = " ++ show e ++ ";"

instance Show VarType where
    show IntType = "Int"
    show DoubleType = "Double"
    show CharType = "Char"

program :: Parser Program
program = Program <$> many function

function :: Parser Function
function = do
    symbol "func"
    id <- identifier
    symbol "("
    params <- separated parameter ","
    symbol ")"
    do
        symbol "->"
        t <- varType
        symbol "="
        b <- block
        symbol "end"
        return (Function id params t b)
        <|> do
            symbol "="
            b <- block
            symbol "end"
            return (Procedure id params b)

parameter :: Parser Parameter
parameter = do
    id <- identifier
    Parameter id <$> varType

block :: Parser Block
block = some statement

statement :: Parser Statement
statement =
    do
        d <- declaration
        symbol ";"
        return (Declaration d)
    <|>
    do
        id <- identifier
        symbol "="
        e <- expr
        symbol ";"
        return (Assign id e)
    <|>
    do
        symbol "if"
        e <- expr
        symbol "then"
        b <- block
        do
            symbol "end"
            return (If e b)
            <|> do
                elif <- statementElseIf
                return (IfElse e b [elif])
            <|> do
                symbol "else"
                b' <- block
                symbol "end"
                return (IfElse e b b')
    <|>
    do
        symbol "while"
        e <- expr
        symbol "do"
        b <- block
        symbol "end"
        return (While e b)
    <|>
    do
        symbol "return"
        e <- expr
        symbol ";"
        return (Return e)

statementElseIf :: Parser Statement
statementElseIf = do
    symbol "else if"
    e <- expr
    symbol "then"
    b <- block
    do
        symbol "end"
        return (If e b)
        <|> do
            elif <- statementElseIf
            return (IfElse e b [elif])
        <|> do
            symbol "else"
            b' <- block
            symbol "end"
            return (IfElse e b b')

declaration :: Parser Declaration
declaration =
    do
        symbol "var"
        id <- identifier
        do
            symbol "="
            DefineInfer id <$> expr
            <|> do
                t <- varType
                do
                    symbol "="
                    Define id t <$> expr
                    <|> return (Declare id t)

varType :: Parser VarType
varType = do
    symbol "Int"; return IntType
    <|> do symbol "Double"; return DoubleType
    <|> do symbol "Char"; return CharType