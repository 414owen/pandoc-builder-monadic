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
  ) where

import Text.Pandoc.Builder.Monadic.Verbatim

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
