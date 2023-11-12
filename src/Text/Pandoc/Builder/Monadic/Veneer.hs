{-|
This module exports extra convenient builders, that don't have equivalents
in pandoc-types' Text.Pandoc.Builder.
-}

module Text.Pandoc.Builder.Monadic.Veneer
  (
  -- * Block list builders
    div'
  , span'
  , h1
  , h2
  , h3
  , h4
  , h5
  , tableWithColspec
  , strshow
  ) where

import Text.Pandoc.Builder.Monadic.Verbatim
  ( Builder, Inline(..), Block(..), ColSpec, header, simpleTable
  , divWith, spanWith, nullAttr, str
  )
import Text.Pandoc.Builder.Monadic.Utils

import qualified Data.Text as T

-- | Build a level 1 header.
h1 :: Builder Inline -> Builder Block
h1 = header 1

-- | Build a level 2 header.
h2 :: Builder Inline -> Builder Block
h2 = header 2

-- | Build a level 3 header.
h3 :: Builder Inline -> Builder Block
h3 = header 3

-- | Build a level 4 header.
h4 :: Builder Inline -> Builder Block
h4 = header 4

-- | Build a level 5 header.
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

-- | Build a generic block container.
-- This would be named 'div', but that clashes with prelude's 'div'.
div' :: Builder Block -> Builder Block
div' = divWith nullAttr

-- | Build a generic inline container.
-- This would be named 'span', but that clashes with prelude's 'span'.
span' :: Builder Inline -> Builder Inline
span' = spanWith nullAttr

-- | Build a v'Str' using a values's `Show` instance.
strshow :: Show a => a -> Builder Inline
strshow = str . T.pack . show
