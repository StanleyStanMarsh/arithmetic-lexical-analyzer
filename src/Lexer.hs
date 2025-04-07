module Lexer 
    ( lexer,
      Token(..),
      assignIdentifierNumbers
    ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- токены
data Token = Identifier String Int
           | IntNumber String
           | FloatNumber String
           | Assign
           | Plus | Minus | Mul | Div
           | LParen | RParen
           | Semicolon
           | Begin | End
           | Error String String
           deriving (Show, Eq)

-- маппинг ключевых слов на уникальные значения (X1, X2, ...)
keywordMapping :: [(String, String)]
keywordMapping = [("begin", "X1"), ("end", "X2")]

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | c == '#'  = lexer (dropWhile (/= '\n') cs)
  | c == ';'  = Semicolon : lexer cs
  | c == ':'  = lexerAssign cs
  | isAlpha c = lexKeywordOrIdentifier (c:cs)
  | isDigit c = lexSignedNumber '+' (c:cs)
  | (c == '+' || c == '-') && not (null cs) && isDigit (head cs) = lexSignedNumber c cs
  | c == '+'  = Plus : lexer cs
  | c == '-'  = Minus : lexer cs
  | c == '*'  = Mul : lexer cs
  | c == '/'  = Div : lexer cs
  | c == '('  = LParen : lexer cs
  | c == ')'  = RParen : lexer cs
  | otherwise = Error "Неизвестный символ " [c] : lexer cs

identifierCheck :: String -> Token
identifierCheck x
    | length x > 16 = Error "Длинный идентификатор" x
    | all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) x = Identifier x 0
    | otherwise = Error "Неверный идентификатор" x

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
keywordToken c = Error "Не ключевое слово" c

-- присваивание
lexerAssign :: String -> [Token]
lexerAssign ('=':xs) = Assign : lexer xs
lexerAssign (x:xs) = Error "Неизвестный символ" [x] : lexer (x:xs)
lexerAssign [] = []

-- числа со знаком
lexSignedNumber :: Char -> String -> [Token]
lexSignedNumber sign xs =
    let (numPart, rest) = span (\ch -> isDigit ch || ch == '.' || ch == 'e' || ch == 'E' || ch == '+' || ch == '-') xs
        -- Если знак положительный, используем numPart, иначе добавляем знак '-' в начало.
        numberStr = if sign == '+' then numPart else sign : numPart
    in if not (null rest) && isAlpha (head rest)
          then let extra = takeWhile isAlphaNum rest
                   errToken = numberStr ++ extra
               in Error "Неверный идентификатор" errToken : lexer (dropWhile isAlphaNum rest)
          else if any (`elem` ".eE") numberStr
                 then case reads numberStr :: [(Float, String)] of
                         [(_, "")] -> FloatNumber numberStr : lexer rest
                         _           -> Error "Неверное вещественное" numberStr : lexer rest
                 else case reads numberStr :: [(Int, String)] of
                         [(_, "")] -> IntNumber numberStr : lexer rest
                         _           -> Error "Неверное целое" numberStr : lexer rest

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
