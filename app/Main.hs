module Main where

import System.Environment (getArgs)
import Parsers.Base (parse)
import Parsers.Program (program)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let res = case parse program contents of
            (x, _):_ -> x
            _otherwise -> error "Failed to parse program"
    print res