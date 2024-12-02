module Lib
    ( isBinary
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

newtype Parser a = Parser { runParser :: Text -> Maybe (Text, a) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    -- хотим применить func над результатом парсера p
    fmap func (Parser p) = Parser f where
        -- парсер f возвращает:
        f origText = case p origText of
            Nothing -> Nothing -- Nothing, если парсер p возвращает Nothing
            Just (remainingP, resP) -> Just (remainingP, func resP) -- (остаток, resP обработанный функцией func), если p возвращает (остаток, resP)

instance Applicative Parser where
    pure :: a -> Parser a
    -- возвращаем всегда (изначальная строка, передаваемое значение)
    pure text = Parser (\orig -> Just(orig, text))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
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

isBinary :: Text -> Bool
isBinary = T.all (`elem` ("01" :: String))

satisfy :: (Text -> Bool) -> Parser Text
satisfy pr = Parser f where
    f cs | pr cs = Just (T.pack "", cs)
    f _ = Nothing

binary :: Parser Text
binary = satisfy isBinary
