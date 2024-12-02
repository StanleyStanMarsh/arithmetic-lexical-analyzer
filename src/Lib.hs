module Lib
    ( isBinary
    , findBinaries
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

isBinary :: Text -> Bool
isBinary = T.all (`elem` ("01" :: String))

findBinaries :: Text -> [Text]
findBinaries input = filter isBinary $ T.words $ T.map replaceNonBinary input
  where
    replaceNonBinary c
      | isDigit c = c
      | otherwise = ' '
