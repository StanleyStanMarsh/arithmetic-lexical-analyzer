module Lexer 
    ( lexer,
      printLexTable
    ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- токены
data Token = Identifier String Int
           | IntNumber Int
           | FloatNumber Float
           | Assign
           | Plus | Minus | Mul | Div
           | LParen | RParen
           | Semicolon
           | Begin | End
           | Error String
           deriving (Show, Eq)

-- маппинг ключевых слов на уникальные значения (X1, X2, ...)
keywordMapping :: [(String, String)]
keywordMapping = [("begin", "X1"), ("end", "X2")]

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | c == ';'  = lexer cs
  | c == '#'  = lexer (dropWhile (/='\n') cs)
  | c == '+'  = Plus : lexer cs
  | c == '-'  = Minus : lexer cs
  | c == '*'  = Mul : lexer cs
  | c == '/'  = Div : lexer cs
  | c == '('  = LParen : lexer cs
  | c == ')'  = RParen : lexer cs
  | c == ':'  = lexerAssign cs
  | isAlpha c = lexKeywordOrIdentifier (c:cs)
  | isDigit c = lexNumber (c:cs)
  | otherwise = Error [c] : lexer cs  -- Ошибка, если символ не распознан

identifierCheck :: String -> Token
identifierCheck x
    | length x > 16 = Error "Identifier too long"
    | all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) x = Identifier x 0
    | otherwise = Error "Identifier contains invalid characters"

lexKeywordOrIdentifier :: String -> [Token]
lexKeywordOrIdentifier xs
  | isKeyWord ident = keywordToken ident : lexer rest
  | otherwise = identifierCheck ident : lexer rest
  where
    -- состоит из букв и цифр, но должен начинаться с буквы
    (ident, rest) = span isAlphaNum xs

isKeyWord :: String -> Bool
isKeyWord x = x `elem` map fst keywordMapping

-- токен для ключевых слов
keywordToken :: String -> Token
keywordToken "begin" = Begin
keywordToken "end" = End
keywordToken _ = Error "Unknown keyword"

-- присваивание
lexerAssign :: String -> [Token]
lexerAssign ('=':xs) = Assign : lexer xs
lexerAssign _ = Error "Invalid assignment operator" : []

-- числа
lexNumber :: String -> [Token]
lexNumber xs =
    let (num, rest) = span isDigit xs
    in case rest of
        ('.':ys) -> lexFloat (num ++ ".") ys  -- если точка, переходим к вещественному
        ('e':ys) -> lexExponent (num ++ "e") ys  -- если есть экспонента, переходим к обработке экспоненты
        ('E':ys) -> lexExponent (num ++ "E") ys  -- для заглавной 'E'
        _        -> IntNumber (read num) : lexer rest  -- целое

-- числа с плавающей точкой
lexFloat :: String -> String -> [Token]
lexFloat xs ys =
    let (fracPart, rest) = span isDigit ys
    in case rest of
        ('e':zs) -> lexExponent (xs ++ fracPart ++ "e") zs
        ('E':zs) -> lexExponent (xs ++ fracPart ++ "E") zs
        _        -> FloatNumber (read (xs ++ fracPart)) : lexer rest

-- обработка экспоненциальной части
lexExponent :: String -> String -> [Token]
lexExponent xs (c:cs)
    | c == '+' || c == '-' =  -- знак экспоненты (e+2, e-3)
        let (expDigits, rest) = span isDigit cs
        in if null expDigits
           then Error "Invalid exponent format" : lexer rest  -- если после e+ или e- нет цифр
           else FloatNumber (read (xs ++ c : expDigits)) : lexer rest
    | isDigit c =
        let (expDigits, rest) = span isDigit (c:cs)
        in FloatNumber (read (xs ++ expDigits)) : lexer rest
    | otherwise = Error "Invalid exponent format" : lexer (c:cs)  -- если e не сопровождается цифрами

lexExponent xs [] = Error "Invalid exponent format" : []  -- если строка обрывается на e

-- присваивание уникальных номеров идентификаторам
assignIdentifierNumbers :: [Token] -> [Token]
assignIdentifierNumbers tokens = assignNumbers [] 1 tokens
  where
    -- вспомогательная функция для обхода списка токенов
    assignNumbers _ _ [] = []
    
    -- обработка идентификаторов
    assignNumbers used count (Identifier name _ : ts)
      | Just num <- lookup name used =
          Identifier name num : assignNumbers used count ts
      | otherwise =
          let newUsed = (name, count) : used
          in Identifier name count : assignNumbers newUsed (count + 1) ts
    
    -- пропуск остальных токенов
    assignNumbers used count (t:ts) = t : assignNumbers used count ts

-- печать таблицы лексем
printLexTable :: String -> IO ()
printLexTable input = do
    putStrLn $ input ++ "\n"
    let tokens = lexer input
    let tokensWithNumbers = assignIdentifierNumbers tokens
    putStrLn "Таблица лексем:\n"
    putStrLn $ "Лексема" ++ tabs ++ "Тип лексемы" ++ tabs ++ "Значение"
    mapM_ printToken tokensWithNumbers

tabs :: String
tabs = "\t\t\t\t"

-- печать отдельного токена
printToken :: Token -> IO ()
printToken (Identifier name num) = putStrLn $ name ++ tabs ++ "Идентификатор" ++ tabs ++ name ++ " : " ++ show num
printToken (IntNumber num) = putStrLn $ show num ++ tabs ++ "Целочисленная константа\t\t\t" ++ show num
printToken (FloatNumber num) = putStrLn $ show num ++ tabs ++ "Вещественная константа\t\t\t" ++ show num
printToken Assign = putStrLn $ ":=" ++ tabs ++ "Знак присваивания"
printToken Plus = putStrLn $ "+" ++ tabs ++ "Знак арифметической операции"
printToken Minus = putStrLn $ "-" ++ tabs ++ "Знак арифметической операции"
printToken Mul = putStrLn $ "*" ++ tabs ++ "Знак арифметической операции"
printToken Div = putStrLn $ "/" ++ tabs ++ "Знак арифметической операции"
printToken LParen = putStrLn $ "(" ++ tabs ++ "Знак операции"
printToken RParen = putStrLn $ ")" ++ tabs ++ "Знак операции"
printToken Begin = putStrLn $ "begin" ++ tabs ++ "Ключевое слово" ++ tabs ++ "X1"
printToken End = putStrLn $ "end" ++ tabs ++ "Ключевое слово" ++ tabs ++ "X2"
printToken (Error msg) = putStrLn $ "Ошибка" ++ tabs ++ msg

