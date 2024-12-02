module Lib
    ( isBinary
    ) where

import Data.Text (Text)
import qualified Data.Text as T

isBinary :: Text -> Bool
isBinary = T.all (`elem` ("01" :: String))
