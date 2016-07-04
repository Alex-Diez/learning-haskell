module Main where

putStrLn' str = do
    putStr str
    putChar '\n'

putQStrLn str = do
    putChar '\"'
    putStr str
    putChar '\"'
    putChar '\n'

main = do
    putStrLn "Enter text: "
    str <- getLine
    putQStrLn str
