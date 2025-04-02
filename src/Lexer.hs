module Lexer
    ( lexer
    ) where

import Data.Char (isDigit, isAlpha, isAlphaNum)

data Token = Identifier String
           | Number String
           | Assign
           | Plus | Minus | Mul | Div
           | LParen | RParen
           | Semicolon
           | Error String
           deriving (Show, Eq)

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
lexer (x:xs) | isAlpha x = lexIdentifier (x:xs)
             | isDigit x = lexNumber (x:xs)
             | otherwise = Error [x] : lexer xs

lexIdentifier :: String -> [Token]
lexIdentifier xs = let (ident, rest) = span isAlphaNum xs
                   in if length ident > 16
                      then Error "Identifier too long" : lexer rest
                      else Identifier ident : lexer rest

lexNumber :: String -> [Token]
lexNumber xs = let (num, rest) = span (`elem` ".0123456789eE-") xs
               in Number num : lexer rest

dropComment :: String -> String
dropComment ('*':'/':xs) = xs
dropComment (_:xs) = dropComment xs
dropComment [] = [] -- незакрытый комментарий, но игнорируем

testInput = "    x := (3+4j) * y; //комментарии могут быть любой длинны 123456789098765432123456789 комекрп\n\
    \y := 5.6 - 7.8j;\
    \x := x ^ 2;"
