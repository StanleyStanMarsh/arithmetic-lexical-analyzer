module Main (main) where

import Lib
import Data.Text (pack)

main :: IO ()
main = do
    print $ findBinaries $ pack "qasldjnasdf 123 asdknlasd 0001010 askdjfn 000011111"
