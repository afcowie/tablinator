module Tablinator.Table
(
    Column(..),
    processObjectStream,
    Alignment(..)
) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
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
processObjectStream ms =
  let
    headers = renderTableHeading ms
    inline  = [Str "This is the caption"]

    hdings  = fmap T.unpack (headings (head ms))
    align   = fmap (const AlignLeft) hdings
    widths  = fmap (const 0) hdings
    rows    = renderTableBody ms
  in
    [Table inline align widths headers rows]


renderTableHeading :: Column k => [Map k Text] -> [[Block]]
renderTableHeading ms =
  let
    hdings  = fmap T.unpack (headings (head ms))

    mkHeading :: String -> [Block]
    mkHeading h = [Plain [Str h]]
  in
    fmap mkHeading hdings

renderTableBody :: Column k => [Map k Text] -> [[[Block]]]
renderTableBody ms =
  let
    mkRow :: Map k Text -> [[Block]]
    mkRow m = (\(_,v) -> [Para [Str $ T.unpack v]]) <$> Map.assocs m
  in
    fmap mkRow ms
