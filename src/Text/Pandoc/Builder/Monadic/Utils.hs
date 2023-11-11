{-# LANGUAGE OverloadedStrings #-}

{-|
Utility functions for working with builders, and constructed pandoc ASTs
-}

module Text.Pandoc.Builder.Monadic.Utils
  ( mapBuilder
  , intersperseTablesWithBlankRows
  , vspace
  ) where

import Data.List                            (intersperse)
import Text.Pandoc.Builder.Monadic.Internal (tellAll, runToDList)
import Text.Pandoc.Builder.Monadic.Verbatim hiding (caption)

-- | Map every element written in a t'Builder'.
-- This is useful if you have laid out bespoke elements, such as
-- 
-- > mapBuilder boldStrings $ do
-- >   div "lorem ipsum "
-- >   str "dolor sit amet"
-- >
-- > boldStrings s@(Str _) = Strong s
-- > boldStrings a = a
--
-- It's also useful for creating custom pandoc builders.
-- See 'Text.Pandoc.Builder.Monadic.Veneer.tableWithColspec'.
mapBuilder :: (a -> b) -> Builder a -> Builder b
mapBuilder f = tellAll . fmap f . runToDList

-- | Intersperse table with blank rows, this is useful for
-- clarity (with some backends) when using multiline cells.
-- This is exposed as a modifier, because there are already
-- too many ways to construct a table.
intersperseTablesWithBlankRows :: Builder Block -> Builder Block
intersperseTablesWithBlankRows = mapBuilder updateBlock
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

-- | Vertical space - a paragraph containing a non-breaking space
vspace :: Builder Block
vspace = para "\x00A0"
