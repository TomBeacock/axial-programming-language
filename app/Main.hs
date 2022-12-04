module Main where

import Parsers.Base (parse)
import Parsers.Expr
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let [(x, xs)] = parse expr contents
    print x