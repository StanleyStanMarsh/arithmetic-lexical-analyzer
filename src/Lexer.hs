module Lexer
    ( lexer
    ) where

import Data.Char (isAlpha, isAlphaNum, isDigit)

-- Определение токенов
data Token = Identifier String
           | Number String
           | Assign
           | Plus | Minus | Mul | Div
           | LParen | RParen
           | Semicolon
           | Begin | End
           | Error String
           deriving (Show, Eq)

-- Основная функция лексического анализа
lexer :: String -> [Token]
lexer [] = []
lexer (' ':xs) = lexer xs
lexer ('\n':xs) = lexer xs
lexer (';':xs) = Semicolon : lexer xs
lexer ('+':xs) = Plus : lexer xs
lexer ('-':xs) = Minus : lexer xs
lexer ('*':xs) = Mul : lexer xs
lexer ('/':'/':xs) = lexer (dropWhile (/='\n') xs) -- однострочный комментарий
lexer ('/':'*':xs) = lexer (dropComment xs) -- многострочный комментарий
lexer ('/':xs) = Div : lexer xs
lexer ('(':xs) = LParen : lexer xs
lexer (')':xs) = RParen : lexer xs
lexer (':':'=':xs) = Assign : lexer xs
lexer (x:xs) | isAlpha x = lexKeywordOrIdentifier (x:xs)
             | isDigit x = lexNumber (x:xs)
             | otherwise = Error [x] : lexer xs

-- Обработка ключевых слов и идентификаторов
lexKeywordOrIdentifier :: String -> [Token]
lexKeywordOrIdentifier xs
  | isKeyWord ident = keywordToken ident : lexer rest
  | otherwise = Identifier ident : lexer rest
  where
    (ident, rest) = span isAlphaNum xs

-- Проверка на ключевое слово
isKeyWord :: String -> Bool
isKeyWord x = x == "begin" || x == "end"

-- Возвращает соответствующий токен для ключевых слов
keywordToken :: String -> Token
keywordToken "begin" = Begin
keywordToken "end" = End
keywordToken _ = Error "Unknown keyword"

-- Обработка чисел
lexNumber :: String -> [Token]
lexNumber xs = let (num, rest) = span (`elem` ".0123456789eE-") xs
               in Number num : lexer rest

-- Функция для удаления комментариев
dropComment :: String -> String
dropComment ('*':'/':xs) = xs
dropComment (_:xs) = dropComment xs
dropComment [] = [] -- незакрытый комментарий, но игнорируем

testInput = "    begin\n\
            \x := 3 + 4; // комментарий\n\
            \end\n"

