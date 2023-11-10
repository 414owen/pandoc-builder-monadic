{-# LANGUAGE FlexibleContexts #-}
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

cell :: Build Block a => a -> Cell
cell = V.simpleCell

cell' :: Build Block a => Alignment -> RowSpan -> ColSpan -> a -> Cell
cell' = V.cell

table :: (Build Block a, Build Block b) => [a] -> [[b]] -> Builder Block
table = V.simpleTable

table'
  :: Caption
  -> [ColSpec]
  -> TableHead
  -> [TableBody]
  -> TableFoot
  -> Builder Block
table' = V.table

caption :: Build Block a => a -> Caption
caption = V.simpleCaption

caption' :: Build Block a => Maybe ShortCaption -> a -> Caption
caption' = V.caption

imgFigure :: Build Inline a => a -> Text -> Text
                  -> Builder Block
imgFigure = V.simpleFigure

imgFigureWith :: Build Inline a => Attr -> a -> Text -> Text -> Builder Block
imgFigureWith = V.simpleFigureWith

