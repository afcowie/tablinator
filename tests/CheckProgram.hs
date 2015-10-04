{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "Satisfaction" $ do
        it "satisifies" $ do
            True `shouldBe` True

