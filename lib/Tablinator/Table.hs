module Tablinator.Table where

import Text.Pandoc

kvPairsToTable :: (String,String) -> Block
kvPairsToTable _ = undefined

example :: Block
example =
  let
    inline  = [Str "This is the caption"]
    align   = [AlignDefault, AlignRight]
    widths  = [0,0]
    headers = [[Plain [Str "First"]], [Plain [Str "Second"]]]
    rows    = [row1]
    row1    = [[Plain [Str "Left"]], [Plain [Str "Right"]]]
  in
    Table inline align widths headers rows

table :: Pandoc
table = Pandoc nullMeta [example]
