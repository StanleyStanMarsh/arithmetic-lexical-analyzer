module Main (main) where

import Lexer
import System.IO
import Control.Monad (unless)

main :: IO ()
main = loop
  where
    loop = do
        putStrLn "Выберите номер строки (1-4) или введите 0 для выхода\n"
        putStrLn $ "1)\n" ++ testInput1 ++ "\n"
        putStrLn $ "2)\n" ++ testInput2 ++ "\n"
        putStrLn $ "3)\n" ++ testInput3 ++ "\n"
        putStrLn $ "4)\n" ++ testInput4 ++ "\n"
        putStrLn "Номер строки: "
        hFlush stdout
        input <- getLine
        case input of
            "1" -> printLexTable testInput1 >> loop
            "2" -> printLexTable testInput2 >> loop
            "3" -> printLexTable testInput3 >> loop
            "4" -> printLexTable testInput4 >> loop
            "0" -> putStrLn "Выход из программы."
            _   -> putStrLn "Некорректный ввод, попробуйте снова." >> loop

testInput1 :: String
testInput1 = "begin\n\
             \x := 3 + 4; # comment\n\
             \y := 5.6 -@!$ 7.8j;;;\n\
             \end"

testInput2 :: String
testInput2 = "x := 12.34 + y - 5.67e-2; # extremely-very long comment 123456678901239238492384"

testInput3 :: String
testInput3 = "begin\n\
             \1.2.3;\n\
             \ident2 := -105;"

testInput4 :: String
testInput4 = "a * (b + c) - a * b - a * c + x / y # all operations"
