module Main (main) where

import Lib
import System.IO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

main :: IO ()
main = do
    hSetEncoding stdout utf8
    putStrLn "Enter file name:"
    fileName <- getLine
    lines <- readFileLines fileName
    -- mapM_ (TE.decodeUtf8 . B.putStrLn) lines
    mapM_ parseAndPrint lines
