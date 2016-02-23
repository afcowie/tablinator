{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tablinator.Table
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc (Alignment(..))

data LibraryBooks
    = Genre
    | Author
    | Title
    | Description
    deriving (Eq, Ord, Show)

instance Column LibraryBooks where
    heading x = T.pack $ show x
    alignment _ = AlignLeft


main :: IO ()
main = T.putStrLn $ heading Author

