module Lib
    ( parseAndPrint
    , readFileLines
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Control.Applicative
import Data.Char (digitToInt, intToDigit)
import Data.Bits ((.&.), (.|.), xor)
import Numeric (showIntAtBase)

newtype Parser a = Parser { runParser :: Text -> Maybe (Text, a) }

instance Functor Parser where
    -- хотим применить func над результатом парсера p
    fmap func (Parser p) = Parser f where
        -- парсер f возвращает:
        f origText = case p origText of
            Nothing -> Nothing -- Nothing, если парсер p возвращает Nothing
            Just (remainingP, resP) -> Just (remainingP, func resP) -- (остаток, resP обработанный функцией func), если p возвращает (остаток, resP)

instance Applicative Parser where
    -- возвращаем всегда (изначальная строка, передаваемое значение)
    pure text = Parser (\orig -> Just(orig, text))

    -- хотим чтобы был парсер, который применяет к остатку 1 парсера 2 парсер,
    -- а затем применяет 1 парсер ко 2
    (Parser u) <*> (Parser v) = Parser f where
        f origText = case u origText of
            Nothing -> Nothing
            -- remainingU - остаток 1 парсера
            Just (remainingU, resU) -> case v remainingU of
                Nothing -> Nothing
                -- remainingV - итоговый остаток, resU применяем над resV
                Just (remainingV, resV) -> Just (remainingV, resU resV)

instance Alternative Parser where
    -- парсер всегда возвращающий Nothing
    empty = Parser $ \_ -> Nothing

    Parser u <|> Parser v = Parser f where
        -- пытаемся применить парсер u
        f origText = case u origText of
            -- если он вернул Nothing, то применям парсер v
            Nothing -> v origText
            -- если вернул какой то результат, то оставляем результат
            res -> res

isBinaryChar :: Char -> Bool
isBinaryChar c =
    c == '0' || c == '1'

isOperation :: Char -> Bool
isOperation c =
    c == '&' || c == '|' || c == '⊕'

isSpace :: Char -> Bool
isSpace c = c == ' '

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f where
    -- берем первый символ
    f cs = case T.uncons cs of
        Nothing -> Nothing
        -- если первый элемент соответствует предикату pr
        Just (fstChar, remainingText)
            | pr fstChar -> Just (remainingText, fstChar) -- то возвращаем (остаток, подоходящий первый элемент)
            | otherwise -> Nothing
    f _ = Nothing

binary :: Parser Text
binary = Parser $ \text ->
    -- если первый элемент
    case runParser (satisfy isBinaryChar) text of
        Nothing -> Nothing -- не подошел, то строка очевидно сразу не подходит
        -- (рекурсивно) если парсер на остатке
        Just (remaining, c) -> case runParser binary remaining of
            Nothing -> Just (remaining, T.singleton c) -- первый элемент не подошел, то берем (старый остаток, единственный подошедший элемент)
            Just (remaining', rest) -> Just (remaining', T.cons c rest) -- сработал штатно, то (новый остаток, добавляем подошедший элемент к остальным)

binToInt :: Text -> Int
binToInt text = T.foldl' (\acc c -> acc * 2 + digitToInt c) 0 text

intToBin :: Int -> Text
intToBin n = T.pack (showIntAtBase 2 intToDigit n "")

binaryInt :: Parser Int
binaryInt = binToInt <$> binary

operation :: Parser Char
operation = satisfy isOperation

oneSpace :: Parser Char
oneSpace = satisfy isSpace

spaces :: Parser Text
spaces = (T.cons) <$> oneSpace <*> spaces <|> pure T.empty

applyOperation :: Char -> Int -> Int -> Int
applyOperation op
    | op == '&' = (.&.)
    | op == '|' = (.|.)
    | op == '⊕' = xor

binaryExpression :: Parser Int
binaryExpression =
    (\_ b1 _ op _ b2 -> applyOperation op b1 b2)
    <$> spaces
    <*> binaryInt
    <*> spaces
    <*> operation
    <*> spaces
    <*> binaryInt

binaryExpressionFormatted :: Parser Text
binaryExpressionFormatted =
    (\_ b1 _ op _ b2 -> formatExpression b1 op b2)
    <$> spaces
    <*> binary
    <*> spaces
    <*> operation
    <*> spaces
    <*> binary

formatExpression :: Text -> Char -> Text -> Text
formatExpression b1 op b2 =
    let int1 = binToInt b1
        int2 = binToInt b2
        result = applyOperation op int1 int2
    in b1 <> (T.pack " ") <> T.singleton op <> (T.pack " ") <> b2 <> (T.pack " = ") <> intToBin result

readFileLines :: FilePath -> IO [T.Text]
readFileLines filePath = fmap (T.lines . TE.decodeUtf8) (B.readFile filePath)

parseAndPrint :: T.Text -> IO ()
parseAndPrint input = 
    case runParser binaryExpressionFormatted input of
        Just (_, result) -> TIO.putStrLn result
        Nothing -> putStrLn "Wrong string"