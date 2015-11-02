module Main where

import System.Environment (getArgs)

import Tablinator.Table
import Text.Pandoc

main :: IO ()
main = do
{-
    args <- getArgs
    let file = head args
    print file
-}
    putStrLn (writeMarkdown def table)
