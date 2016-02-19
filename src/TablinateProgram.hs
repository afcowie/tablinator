module Main where

{-
import System.Environment (getArgs)
import Tablinator.Table
-}
import Text.Pandoc
import Text.Pandoc.PDF
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map.Lazy as Map

main :: IO ()
main = do
{-
    args <- getArgs
    let file = head args
    print file
-}
    template <- L.readFile "engine/pdflatex.template"
    header   <- L.readFile "engine/header.sty"
    let options = def {
        writerVerbose = False,
        writerStandalone = True,
        writerTemplate = L.unpack template,
        writerVariables = [("header-includes", L.unpack header)]
    }

    result <- makePDF "pdflatex" writeLaTeX options table
    case result of
        Left err  -> L.putStrLn err
        Right pdf -> L.writeFile "junk.pdf" pdf
    

example :: Block
example =
  let
    inline  = [Str "This is the caption"]
    align   = [AlignDefault, AlignLeft]
    widths  = [0,0]
    headers = [[Plain [Str "First"]], [Plain [Str "Second"]]]
    rows    = [row1, row2]
    row1    = [[Para [Str "Left"]], [Para [Str "Right"]]]
    row2    = [[Para [Str "And then"]],
               [Para [Code nullAttr "printf(\"Hello World\\n\");"]]]
  in
    Table inline align widths headers rows

meta :: Meta
meta = Meta Map.empty

table :: Pandoc
table = Pandoc meta [example]
