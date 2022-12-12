{-# LANGUAGE LambdaCase #-}
module Parsers.Base where

import GHC.Base ((<|>), Alternative, empty, some, many)
import Data.Char (isSpace, isDigit)

-- Parser type
newtype Parser a = P (String -> [(a, String)])
type Identifier = String

parse :: Parser a -> String ->  [(a, String)]
parse (P p) = p

instance Functor Parser where
    fmap f p = P (\inp -> [(f x, s) | (x, s) <- parse p inp])

instance Applicative Parser where
    pure a = P(\inp -> [(a, inp)])
    pf <*> pa = P (\inp -> case parse pf inp of
        [(x, out)] -> parse (fmap x pa) out
        _otherwise -> [])

instance Monad Parser where
  pa >>= f = P (\inp -> [res | (x, out) <- parse pa inp, res <- parse (f x) out])

instance Alternative Parser where
    empty = P (const [])
    pa <|> pb = P (\inp -> case parse pa inp of
        [] -> parse pb inp
        xs -> xs)

-- Basic parsers
item :: Parser Char
item = P (\case
    [] -> []
    (x:xs) -> [(x, xs)])

itemWhere :: (Char -> Bool) -> Parser Char
itemWhere f = do
    c <- item
    if f c then return c else empty

space :: Parser ()
space = do many $ itemWhere isSpace; return ()

token :: Parser a -> Parser a
token p = do space; a <- p; space; return a

char :: Char -> Parser Char
char c = itemWhere (== c)

string :: String -> Parser String
string [] = return ""
string (x:xs) = do c <- char x; s <- string xs; return (c:s)

symbol :: String -> Parser String
symbol xs = do space; ys <- string xs; space; return ys

separated :: Parser a -> String -> Parser [a]
separated p s = (do
            x <- p
            do
                symbol s
                xs <- separated p s
                return (x:xs)
                <|> return [x])
            <|> return []

digit :: Parser Char
digit = itemWhere isDigit

natural :: Parser Int
natural = do xs <- some digit; return (read xs)

real :: Parser Double
real = do
    xs <- many digit
    do
        char '.'; ys <- some digit; return (read (xs ++ "." ++ ys))
        <|> return (read xs)
    <|> do
        char '.'; xs <- some digit; return (read ("." ++ xs))

identifier :: Parser Identifier
identifier = do token identifier'
    where
        identifier' = do
            xs <- many $ itemWhere (== '_')
            y <- itemWhere isAsciiAlpha
            zs <- many $ itemWhere (\c -> isAsciiAlphaNum c || c == '_')
            return (xs ++ [y] ++ zs)

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || c `elem` ['0'..'9']