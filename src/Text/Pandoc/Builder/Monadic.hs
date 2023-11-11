{-|
This module exports an API similar to pandoc-types' Text.Pandoc.Builder, but
with the `simple*` versions as the default versions, and with a few extras
from "Text.Pandoc.Builder.Monadic.Veneer".

In general, Builder.simple{a} becomes Builder.Monadic.{a}
and Builder.{a} becomes Builder.Monadic.{a}'.
-}

module Text.Pandoc.Builder.Monadic
  ( 
  -- * Monadic versions of functions in "Text.Pandoc.Builder"
    module Text.Pandoc.Builder.Monadic.Verbatim
  -- * Extra builders
  , module Text.Pandoc.Builder.Monadic.Veneer
  -- * Extra builders
  , module Text.Pandoc.Builder.Monadic
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

-- | Build a 1x1 cell with default alignment, given some pandoc.
-- Same as 'Text.Pandoc.Builder.Monadic.Verbatim.simpleCell'.
cell :: Builder Block -> Cell
cell = V.simpleCell

-- | Build a cell of a table, full API excluding attributes.
-- Same as 'Text.Pandoc.Builder.Monadic.Verbatim.cell'.
cell' :: Alignment -> RowSpan -> ColSpan -> Builder Block -> Cell
cell' = V.cell

-- | Build a table, given a list of header cells, and a list of rows.
-- Same as 'Text.Pandoc.Builder.Monadic.Verbatim.simpleTable'.
table :: [Builder Block] -> [[Builder Block]] -> Builder Block
table = V.simpleTable

-- | Build a table, given attributes, a list of header cells, and a list of rows.
-- This is equivalent to 'Text.Pandoc.Builder.Monadic.Verbatim.simpleTable' with
-- attributes.
tableWith :: Attr -> [Builder Block] -> [[Builder Block]] -> Builder Block
tableWith attr headings body =
  case runToList $ table headings body of
    [Table _ a b c d e] -> tellOne $ Table attr a b c d e
    _ -> error "Invariant broken. Table builder didn't return one element."

-- | Build a table, full API excluding attributes.
-- Same as 'Text.Pandoc.Builder.Monadic.Verbatim.table'.
table'
  :: Caption
  -> [ColSpec]
  -> TableHead
  -> [TableBody]
  -> TableFoot
  -> Builder Block
table' = V.table

-- | Build a table, full API including attributes.
-- Same as 'Text.Pandoc.Builder.Monadic.Verbatim.tableWith'.
tableWith'
  :: Attr
  -> Caption
  -> [ColSpec]
  -> TableHead
  -> [TableBody]
  -> TableFoot
  -> Builder Block
tableWith' = V.tableWith

-- | Make a caption, without a short version.
-- Same as 'Text.Pandoc.Builder.Monadic.Verbatim.simpleCaption'.
caption :: Builder Block -> Caption
caption = V.simpleCaption

-- | Make a caption, with an optional short version.
-- Same as 'Text.Pandoc.Builder.Monadic.Verbatim.caption'.
caption' :: Maybe ShortCaption -> Builder Block -> Caption
caption' = V.caption

-- | Build a captioned figure, containing an image.
-- This is available in pandoc-types >= 1.22.1, which corresponds to pandoc >= 2.15.
imgFigure :: Builder Inline -> Text -> Text
                  -> Builder Block
imgFigure = V.simpleFigure

-- | Build a captioned figure containing an image, with attributes.
-- This is available in pandoc-types >= 1.22.1, which corresponds to pandoc >= 2.15.
imgFigureWith :: Attr -> Builder Inline -> Text -> Text -> Builder Block
imgFigureWith = V.simpleFigureWith
