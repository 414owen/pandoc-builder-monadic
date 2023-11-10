-- | This module exports an API similar to pandoc-types' Text.Pandoc.Builder, but
-- with the `simple*` versions as the default versions, and with a few extras
-- from Text.Pandoc.Builder.Monadic.Veneer.

module Text.Pandoc.Builder.Monadic
  ( module Text.Pandoc.Builder.Monadic.Verbatim
  , module Text.Pandoc.Builder.Monadic.Veneer
  , module Text.Pandoc.Builder.Monadic
  -- * Block list builders
  ) where

import Data.Text                            (Text)
import Text.Pandoc.Builder.Monadic.Verbatim hiding
  ( simpleCell, cell, table, simpleTable
  , caption, simpleCaption, simpleFigure
  , simpleFigureWith, tableWith
  )
import Text.Pandoc.Builder.Monadic.Internal (tellOne, runToList)
import Text.Pandoc.Builder.Monadic.Veneer

import qualified Text.Pandoc.Builder.Monadic.Verbatim as V

-- These are renamed functions
-- where Builder.simple{a} -> Builder.Monadic->{a}
-- and Builder.{a} -> Builder.Monadic->{a}'

cell :: Builder Block -> Cell
cell = V.simpleCell

cell' :: Alignment -> RowSpan -> ColSpan -> Builder Block -> Cell
cell' = V.cell

table :: [Builder Block] -> [[Builder Block]] -> Builder Block
table = V.simpleTable

tableWith :: Attr -> [Builder Block] -> [[Builder Block]] -> Builder Block
tableWith attr headings body =
  case runToList $ table headings body of
    [Table _ a b c d e] -> tellOne $ Table attr a b c d e
    _ -> error "Invariant broken. Table builder didn't return one element."

table'
  :: Caption
  -> [ColSpec]
  -> TableHead
  -> [TableBody]
  -> TableFoot
  -> Builder Block
table' = V.table

tableWith'
  :: Attr
  -> Caption
  -> [ColSpec]
  -> TableHead
  -> [TableBody]
  -> TableFoot
  -> Builder Block
tableWith' = V.tableWith

caption :: Builder Block -> Caption
caption = V.simpleCaption

caption' :: Maybe ShortCaption -> Builder Block -> Caption
caption' = V.caption

imgFigure :: Builder Inline -> Text -> Text
                  -> Builder Block
imgFigure = V.simpleFigure

imgFigureWith :: Attr -> Builder Inline -> Text -> Text -> Builder Block
imgFigureWith = V.simpleFigureWith
