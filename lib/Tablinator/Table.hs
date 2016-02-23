module Tablinator.Table
(
    Column(..),
    processObjectStream
) where

import Data.List (sort)
import Data.Map.Strict (Map, keys, assocs)
import Data.Text (Text, unpack)
import Text.Pandoc

--
-- | A table column. You need to specify an Ord instance which will determine
-- the order of your columns, and implement the 'heading' method to give a
-- heading for each column in the output table
--
class Ord a => Column a where
    heading :: a -> Text

headings :: Column k => Map k a -> [Text]
headings m = fmap heading $ sort $ keys m

--
-- | Given a stream (at present modelled as a list) of input data objects (each
-- represented as a Map), pivot into a list of compound pandoc Blocks, suitable
-- for subsequent emplacement in a Pandoc document.
--
processObjectStream :: Column k => [Map k Text] -> [Block]
processObjectStream mps = let
  hdings  = unpack <$> headings (head mps)
  inline  = [Str "This is the caption"]
  align   = const AlignLeft <$> hdings
  widths  = const 0 <$> hdings
  mkHeader :: String -> [Block]
  mkHeader h = [Plain [Str h]]
  headers = mkHeader <$> hdings
  mkRow :: Map k Text -> [[Block]]
  mkRow mp = (\(_,v) -> [Para [Str $ unpack v]]) <$> assocs mp
  rows = fmap mkRow mps
  --row1    = [[Para [Str "Left"]], [Para [Str "Right"]]]
  --row2    = [[Para [Str "And then"]],
  --           [Para [Code nullAttr "printf(\"Hello World\\n\");"]]]
    in
      [Table inline align widths headers rows]
