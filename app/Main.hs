module Main (main) where

import Lexer
import System.IO

main :: IO ()
main = do
    let testInput1 = "begin\n\
                     \x := 3 + 4; # комментарий\n\
                     \y := 5.6 -@!$ 7.8j;;;\n\
                     \end"
    let testInput2 = "x := 12.34 + y - 5.67e-2; # This is a comment"
    let testInput3 = "begin\n\
                     \1.2.3;\n\
                     \ident2 := -105;"
    putStrLn "input 1:"
    printLexTable testInput1
    putStrLn "input 2:"
    printLexTable testInput2
    putStrLn "input 3:"
    printLexTable testInput3
