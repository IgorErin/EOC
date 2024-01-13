module Main (main) where

import Lang (toString)

main :: IO ()
main = do
    code <- getLine

    putStr $ toString code
