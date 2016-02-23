module Tablinator.Table
(
    Column(..),
    processObjectStream,
    Alignment(..)
) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack)
import Text.Pandoc

--
-- | A table column. You need to specify an Ord instance which will determine
-- the order of your columns, and implement the 'heading' method to give a
-- heading for each column in the output table
--
class Ord a => Column a where
    heading   :: a -> Text
    alignment :: a -> Alignment

headings :: Column k => Map k a -> [Text]
headings m = fmap heading $ Map.keys m

--
-- | Given a stream (at present modelled as a list) of input data objects (each
-- represented as a Map), pivot into a list of compound pandoc Blocks, suitable
-- for subsequent emplacement in a Pandoc document.
--
processObjectStream :: Column k => [Map k Text] -> [Block]
processObjectStream mps = let
  hdings  = fmap unpack (headings (head mps))
  inline  = [Str "This is the caption"]
  align   = fmap (const AlignLeft) hdings
  widths  = fmap (const 0) hdings
  mkHeader :: String -> [Block]
  mkHeader h = [Plain [Str h]]
  headers = fmap mkHeader hdings
  mkRow :: Map k Text -> [[Block]]
  mkRow mp = (\(_,v) -> [Para [Str $ unpack v]]) <$> Map.assocs mp
  rows = fmap mkRow mps
    in
      [Table inline align widths headers rows]
