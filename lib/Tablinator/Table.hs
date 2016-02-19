module Tablinator.Table
(
    Column,
    processObjectStream
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Text.Pandoc (Pandoc, Block)

--
-- | A table column. You need to specify an Ord instance which will determine
-- the order of your columns, and implement the 'heading' method to give a
-- heading for each column in the output table
--
class Ord a => Column a where
    heading :: a -> Text


kvPairsToTable :: [(String,String)] -> Block
kvPairsToTable _ = undefined


--
-- | Given a stream (at present modelled as a list) of input data objects (each
-- represented as a Map), pivot into a list of compound pandoc Blocks, suitable
-- for subsequent emplacement in a Pandoc document.
--
processObjectStream :: Column k => [Map k Text] -> [Block]
processObjectStream = undefined


