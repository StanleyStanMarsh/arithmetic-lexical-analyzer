module Main (main) where

import Printer
import System.IO
import System.Directory (doesFileExist)

main :: IO ()
main = loop
  where
    loop = do
        putStrLn "Выберите номер файла для анализа (1-4) или введите 0 для выхода:\n"
        putStrLn "1) input1.txt"
        putStrLn "2) input2.txt"
        putStrLn "3) input3.txt"
        putStrLn "4) input4.txt"
        putStr   "Номер файла: "
        hFlush stdout
        input <- getLine
        case input of
            "1" -> processFile "input1.txt" >> loop
            "2" -> processFile "input2.txt" >> loop
            "3" -> processFile "input3.txt" >> loop
            "4" -> processFile "input4.txt" >> loop
            "0" -> putStrLn "Выход из программы."
            _   -> putStrLn "Некорректный ввод, попробуйте снова." >> loop

-- Чтение файла и вызов лексера
processFile :: FilePath -> IO ()
processFile path = do
    exists <- doesFileExist path
    if exists
        then do
            content <- readFile path
            printLexTable content
        else
            putStrLn $ "Файл не найден: " ++ path
