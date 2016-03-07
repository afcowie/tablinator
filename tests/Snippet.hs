{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc

import Tablinator.Table

data LibraryBooks
    = Genre
    | Author
    | Title
    | Description
    deriving (Eq, Ord, Bounded, Enum, Show)

instance Column LibraryBooks where
    heading x = T.pack $ show x
    alignment _ = AlignLeft


example :: [Map LibraryBooks Text]
example =
  let
    one = Map.fromList
           [(Author, "Raymond E. Feist"),
            (Genre,"Fantasy"),
            (Description, "Pug and Thomas"),
            (Title,"Magician")]
    two = Map.fromList
           [(Title,"Wuthering Heights"),
            (Author, "Emily Brontë"),
            (Genre,"Gothic"),
            (Description, "It's all about Heathcliffe ")]
  in
    [one,two]

document :: Pandoc
document = Pandoc nullMeta [processObjectStream allColumns example]

main :: IO ()
main = putStrLn $ writeMarkdown def document

