module Text.Pandoc.Builder.Monadic.Utils
  ( mapBuilder
  , intersperseTableWithBlankRows
  ) where

import Data.List (intersperse)
import Text.Pandoc.Builder.Monadic.Internal (Builder, tellAll, runToDList,)
import Text.Pandoc.Builder (Block(..), TableBody(..), Row(..), nullAttr)

mapBuilder :: (a -> b) -> Builder a -> Builder b
mapBuilder f = tellAll . fmap f . runToDList

-- | Intersperse table with blank rows, this is useful for
-- clarity (in some backends) when using multiline cells
intersperseTableWithBlankRows :: Builder Block -> Builder Block
intersperseTableWithBlankRows = mapBuilder updateBlock
  where
    updateBlock :: Block -> Block
    updateBlock block = case block of
      Table attr caption colspec tableHead tableBodies tableFoot ->
        Table attr caption colspec tableHead (updateBody <$> tableBodies) tableFoot
      a -> a

    updateBody :: TableBody -> TableBody
    updateBody (TableBody attr rowHeadCols headRows rows)
      = TableBody attr rowHeadCols headRows $ intersperse blankRow rows

    blankRow :: Row
    blankRow = Row nullAttr mempty
