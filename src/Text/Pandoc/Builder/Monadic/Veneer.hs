-- | This module exports extras / convenience functions, that aren't present
-- in pandoc-types' Text.Pandoc.Builder.

module Text.Pandoc.Builder.Monadic.Veneer
  (
  -- * Block list builders
    h1
  , h2
  , h3
  , h4
  , h5
  , tableWithColspec
  ) where

import Text.Pandoc.Builder.Monadic.Verbatim
  ( Builder, Inline, Block(..), ColSpec, header, simpleTable
  )
import Text.Pandoc.Builder.Monadic.Utils

h1 :: Builder Inline -> Builder Block
h1 = header 1

h2 :: Builder Inline -> Builder Block
h2 = header 2

h3 :: Builder Inline -> Builder Block
h3 = header 3

h4 :: Builder Inline -> Builder Block
h4 = header 4

h5 :: Builder Inline -> Builder Block
h5 = header 5

-- | Colspans seem to be quite important for controlling
-- table layout, so much so that a version of
-- `Builder.SimpleTable` which takes a [ColSpan] seemed
-- in order.
tableWithColspec :: [ColSpec] -> [Builder Block] -> [[Builder Block]] -> Builder Block
tableWithColspec colspec headers rows = mapBuilder f $ simpleTable headers rows
  where
    f :: Block -> Block
    f (Table attr caption _ tableHead tableBodies tableFoot)
      = Table attr caption colspec tableHead tableBodies tableFoot
    f a = a
