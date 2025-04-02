module Main (main) where

import Parser
import System.IO
import qualified Data.Text as T

main :: IO ()
main = do
    let input = "x := 12.34 + y - 5.67e-2; # This is a comment"
    case Parser.lexer (T.pack input) of
        Left err -> putStrLn ("Error: " ++ T.unpack err)
        Right tokens -> print tokens
