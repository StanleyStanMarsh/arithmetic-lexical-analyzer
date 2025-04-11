module Printer
    ( printLexTable
    ) where

import Lexer

-- дополнение строки пробелами справа до заданной длины.
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- ширина столбцов
colWidth1, colWidth2, colWidth3 :: Int
colWidth1 = 30 
colWidth2 = 40
colWidth3 = 40

underscoresCol1, underscoresCol2 :: String
underscoresCol1 = replicate 29 '_'
underscoresCol2 = replicate 39 '_'

-- Печать таблицы лексем
printLexTable :: String -> IO ()
printLexTable input = do
    putStrLn input
    let tokens = lexer input
    let tokensWithNumbers = assignIdentifierNumbers tokens
    putStrLn "\nТаблица лексем:\n"
    putStrLn $ padRight colWidth1 "| Лексема" ++ padRight colWidth2 "| Тип лексемы" ++ padRight colWidth3 "| Значение" ++ "|"
    putStrLn $ "|" ++ underscoresCol1 ++ "|" ++ underscoresCol2 ++ "|" ++ underscoresCol2 ++ "|"
    mapM_ printToken tokensWithNumbers

-- Печать отдельного токена
printToken :: Token -> IO ()
printToken (Identifier name num) = 
    putStrLn $ padRight colWidth1 ("| " ++ name) 
             ++ padRight colWidth2 "| Идентификатор" 
             ++ padRight colWidth3 ("| " ++ name ++ " : " ++ show num) ++ "|"
printToken (IntNumber num) = 
    putStrLn $ padRight colWidth1 ("| " ++ num) 
             ++ padRight colWidth2 "| Целочисленная константа" 
             ++ padRight colWidth3 ("| " ++ num) ++ "|"
printToken (FloatNumber num) = 
    putStrLn $ padRight colWidth1 ("| " ++ num) 
             ++ padRight colWidth2 "| Вещественная константа" 
             ++ padRight colWidth3 ("| " ++ num) ++ "|"
printToken Assign = 
    putStrLn $ padRight colWidth1 "| :=" 
             ++ padRight colWidth2 "| Знак присваивания" 
             ++ padRight colWidth3 "|" ++ "|"
printToken Semicolon = 
    putStrLn $ padRight colWidth1 "| ;" 
             ++ padRight colWidth2 "| Знак разделителя" 
             ++ padRight colWidth3 "|" ++ "|"
printToken Plus = 
    putStrLn $ padRight colWidth1 "| +" 
             ++ padRight colWidth2 "| Знак арифметической операции" 
             ++ padRight colWidth3 "|" ++ "|"
printToken Minus = 
    putStrLn $ padRight colWidth1 "| -" 
             ++ padRight colWidth2 "| Знак арифметической операции" 
             ++ padRight colWidth3 "|" ++ "|"
printToken Mul = 
    putStrLn $ padRight colWidth1 "| *" 
             ++ padRight colWidth2 "| Знак арифметической операции" 
             ++ padRight colWidth3 "|" ++ "|"
printToken Div = 
    putStrLn $ padRight colWidth1 "| /" 
             ++ padRight colWidth2 "| Знак арифметической операции" 
             ++ padRight colWidth3 "|" ++ "|"
printToken LParen = 
    putStrLn $ padRight colWidth1 "| (" 
             ++ padRight colWidth2 "| Открывающая скобка" 
             ++ padRight colWidth3 "|" ++ "|"
printToken RParen = 
    putStrLn $ padRight colWidth1 "| )" 
             ++ padRight colWidth2 "| Закрывающая скобка" 
             ++ padRight colWidth3 "|" ++ "|"
printToken Begin = 
    putStrLn $ padRight colWidth1 "| begin" 
             ++ padRight colWidth2 "| Ключевое слово" 
             ++ padRight colWidth3 "| X1" ++ "|"
printToken End = 
    putStrLn $ padRight colWidth1 "| end" 
             ++ padRight colWidth2 "| Ключевое слово" 
             ++ padRight colWidth3 "| X2" ++ "|"
printToken (Error msg sym) = 
    putStrLn $ padRight colWidth1 ("| " ++ sym)
             ++ padRight colWidth2 ("| " ++ msg)
             ++ padRight colWidth3 "| ОШИБКА" ++ "|"
