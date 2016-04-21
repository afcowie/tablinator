{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.Hspec

import Tablinator.Table

data LibraryBooks
    = Genre
    | Author
    | Title
    | Description
    deriving (Eq, Ord, Enum, Show)

instance Column LibraryBooks where
    heading x   = T.pack $ show x
    alignment x = AlignLeft

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "Column names" $ do
        it "can be specified by a sum type" $ do
            heading Author `shouldBe` "Author"
