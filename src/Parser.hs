module Parser
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
import Numeric (showIntAtBase)

newtype Parser a = Parser { runParser :: Text -> Either Text (Text, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err -> Left err
        Right (rest, result) -> Right (rest, f result)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> case p1 input of
        Left err -> Left err
        Right (rest, f) -> case p2 rest of
            Left err -> Left err
            Right (rest', x) -> Right (rest', f x)

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> case p input of
        Left err -> Left err
        Right (rest, result) -> runParser (f result) rest