{-# LANGUAGE ScopedTypeVariables #-}

module Tablinator.Table
(
    Column(..),
    processObjectStream,
    allColumns,
    Alignment(..)
) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc

--
-- | A table column. Columns are to be unique.  You need to specify an Ord
-- instance which will determine the order of your columns, and implement the
-- 'heading' method to give a heading for each column in the output table.
-- 'alignment' will default to 'AlignLeft'; override at will.
--
class (Ord a, Enum a) => Column a where
    heading   :: a -> Text

    alignment :: a -> Alignment
    alignment _ = AlignLeft


--
-- | Render all the columns of your type. This is a convenience function which
-- gives you all the columns of your 'Enum', in 'Ord' order.
--
allColumns :: Column k => [k]
allColumns = enumFrom $ toEnum 0


--
-- | Given a stream (at present modelled as a list) of input data objects (each
-- represented as a Map), pivot into a compound pandoc Block, suitable for
-- subsequent emplacement in a Pandoc document. You pass in the list of columns
-- from your Column type that you want rendered and then the records to be
-- rendered; use 'allColumns' as a convenience if you want all of them in 'Ord'
-- order.
--
processObjectStream :: Column k => [k] -> [Map k Text] -> Block
processObjectStream order ms =
  let
    heading = renderTableHeading order
    body    = renderTableBody order ms
    result  = heading body
  in
    result

type TableRow = [TableCell]

--
-- Return a function that expects a table body and results in a table.
-- Partial application here; rows left to be supplied to Table constructor.
--
renderTableHeading :: Column k => [k] -> ([TableRow] -> Block)
renderTableHeading columns =
  let
    inline  = [Str "This is the caption"]

    align   = fmap alignment columns

    widths  = fmap (const 0) columns

    readerHeader :: Text -> [Block]
    readerHeader h =
        [Plain [Str $ T.unpack h]]

    headers = fmap (readerHeader . heading) columns
  in
    Table inline align widths headers -- rows

--
-- And at last render cells. The type of the renderColumn function makes sense
-- when you realize that TableCell is a typealias for [Block]. Default to empty
-- if the field is missing from the row.
--
renderTableBody :: forall k. Column k => [k] -> [Map k Text] -> [TableRow]
renderTableBody columns ms =
    fmap (renderTableRow columns) ms
  where
    renderTableRow :: [k] -> Map k Text -> TableRow
    renderTableRow columns m = fmap (renderColumn m) columns

    renderColumn :: Map k Text -> k -> TableCell
    renderColumn m column =
      let
        v = case Map.lookup column m of
                Just value -> value
                Nothing    -> T.empty
      in
        [Para [Str $ T.unpack v]]
