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
  , simpleFigureWith
  )
import Text.Pandoc.Builder.Monadic.Veneer

import qualified Text.Pandoc.Builder.Monadic.Verbatim as V

cell :: Builder Block -> Cell
cell = V.simpleCell

cell' :: Alignment -> RowSpan -> ColSpan -> Builder Block -> Cell
cell' = V.cell

table :: [Builder Block] -> [[Builder Block]] -> Builder Block
table = V.simpleTable

table'
  :: Caption
  -> [ColSpec]
  -> TableHead
  -> [TableBody]
  -> TableFoot
  -> Builder Block
table' = V.table

caption :: Builder Block -> Caption
caption = V.simpleCaption

caption' :: Maybe ShortCaption -> Builder Block -> Caption
caption' = V.caption

imgFigure :: Builder Inline -> Text -> Text
                  -> Builder Block
imgFigure = V.simpleFigure

imgFigureWith :: Attr -> Builder Inline -> Text -> Text -> Builder Block
imgFigureWith = V.simpleFigureWith

