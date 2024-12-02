module Lib
    ( isBinary
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

newtype Parser a = Parser { runParser :: Text -> Maybe (Text, a) }

isBinary :: Text -> Bool
isBinary = T.all (`elem` ("01" :: String))

satisfy :: (Text -> Bool) -> Parser Text
satisfy pr = Parser f where
    f cs | pr cs = Just (T.pack "", cs)
    f _ = Nothing

