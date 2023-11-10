module Text.Pandoc.Builder.Monadic.Utils
  ( mapBuilder
  ) where

import Text.Pandoc.Builder.Monadic.Internal

mapBuilder :: (a -> b) -> Builder a -> Builder b
mapBuilder f = tellAll . fmap f . runToDList
