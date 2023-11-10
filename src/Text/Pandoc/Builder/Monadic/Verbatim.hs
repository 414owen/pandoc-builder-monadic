{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports a 1:1 monadic version of pandoc-types' Text.Pandoc.Builder.

module Text.Pandoc.Builder.Monadic.Verbatim
  ( module Text.Pandoc.Definition
  , Builder
  , Build(..)

  -- * Top-level
  , doc
  , B.setTitle
  , B.setAuthors
  , B.setDate
  , B.setMeta

  -- * Inline list builders
  , text
  , str
  , emph
  , underline
  , strong
  , strikeout
  , superscript
  , subscript
  , smallcaps
  , singleQuoted
  , doubleQuoted
  , cite
  , codeWith
  , code
  , space
  , softbreak
  , linebreak
  , math
  , displayMath
  , rawInline
  , link
  , linkWith
  , image
  , imageWith
  , note
  , spanWith
  , trimInlines

  -- * Block list builders
  , para
  , plain
  , lineBlock
  , codeBlockWith
  , codeBlock
  , rawBlock
  , blockQuote
  , bulletList
  , orderedListWith
  , orderedList
  , definitionList
  , header
  , headerWith
  , horizontalRule
  , cell
  , simpleCell
  , emptyCell
  , cellWith
  , table
  , simpleTable
  , tableWith
#if MIN_VERSION_pandoc_types(1,23,0)
  , figure
  , figureWith
#endif
  , caption
  , simpleCaption
  , emptyCaption
#if MIN_VERSION_pandoc_types(1,22,1)
  , simpleFigureWith
  , simpleFigure
#endif
  , divWith

  -- * Table processing
  , B.normalizeTableHead
  , B.normalizeTableBody
  , B.normalizeTableFoot
  , B.placeRowSection
  , B.clipRows
  ) where

import Control.Arrow               ((***))
import Data.Text                   (Text)

import Text.Pandoc.Definition

import Text.Pandoc.Builder.Monadic.Internal
  ( Builder
  , runToList
  , buildMany
  , tellOne
  )

import qualified Text.Pandoc.Builder as B
import qualified Data.Text           as Text


class Build el a where
  buildToList :: a -> [el]

  buildToMany :: a -> B.Many el
  buildToMany = B.fromList . buildToList

instance Build a (Builder a) where
  buildToList = runToList

instance Build a [a] where
  buildToList = id

instance Build a (B.Many a) where
  buildToList = B.toList
  buildToMany = id

instance Build el () where
  buildToList _ = []

instance Build Inline Text where
  buildToList s = [B.Str s]

instance Build Inline String where
  buildToList = buildToList . Text.pack
  buildToMany = buildToMany . Text.pack


doc :: Build Block a => a -> Pandoc
doc = B.Pandoc mempty . buildToList

text :: Text -> Builder Inline
text = buildMany . B.text

str :: Text -> Builder Inline
str = tellOne . B.Str

liftWrapper :: Build el a => ([el] -> b) -> a -> Builder b
liftWrapper f = tellOne . f . buildToList

emph :: Build Inline a => a -> Builder Inline
emph = liftWrapper B.Emph

underline :: Build Inline a => a -> Builder Inline
underline = liftWrapper B.Underline

strong :: Build Inline a => a -> Builder Inline
strong = liftWrapper B.Strong

strikeout :: Build Inline a => a -> Builder Inline
strikeout = liftWrapper B.Strikeout

superscript :: Build Inline a => a -> Builder Inline
superscript = liftWrapper B.Superscript

subscript :: Build Inline a => a -> Builder Inline
subscript = liftWrapper B.Subscript

smallcaps :: Build Inline a => a -> Builder Inline
smallcaps = liftWrapper B.SmallCaps

singleQuoted :: Build Inline a => a -> Builder Inline
singleQuoted = liftWrapper $ B.Quoted B.SingleQuote

doubleQuoted :: Build Inline a => a -> Builder Inline
doubleQuoted = liftWrapper $ B.Quoted B.DoubleQuote

cite :: Build Inline a => [B.Citation] -> a -> Builder Inline
cite citations = liftWrapper $ B.Cite citations

codeWith :: B.Attr -> Text -> Builder Inline
codeWith = (tellOne .) . B.Code

code :: Text -> Builder Inline
code = codeWith B.nullAttr

space :: Builder Inline
space = tellOne B.Space

softbreak :: Builder Inline
softbreak = tellOne B.SoftBreak

linebreak :: Builder Inline
linebreak = tellOne B.LineBreak

math :: Text -> Builder Inline
math = tellOne . B.Math B.InlineMath

displayMath :: Text -> Builder Inline
displayMath = tellOne . B.Math B.DisplayMath

rawInline :: Text -> Text -> Builder Inline
rawInline format = tellOne . B.RawInline (B.Format format)

link :: Build Inline a => Text -> Text -> a -> Builder Inline
link = linkWith B.nullAttr

linkWith :: Build Inline a => B.Attr -> Text -> Text -> a -> Builder Inline
linkWith attr url title x = tellOne $ B.Link attr (buildToList x) (url, title)

image :: Build Inline a => Text -> Text -> a -> Builder Inline
image = imageWith B.nullAttr

imageWith :: Build Inline a => B.Attr -> Text -> Text -> a -> Builder Inline
imageWith attr url title x = tellOne $ B.Image attr (buildToList x) (url, title)

note :: Build Block a => a -> Builder Inline
note = liftWrapper B.Note

spanWith :: Build Inline a => B.Attr -> a -> Builder Inline
spanWith attr = liftWrapper $ B.Span attr

trimInlines :: Build Inline a => a -> Builder Inline
trimInlines = buildMany . B.trimInlines . buildToMany

-- Block list builders

para :: Build Inline a => a -> Builder Block
para = liftWrapper B.Para

plain :: Build Inline a => a -> Builder Block
plain = buildMany . B.plain . buildToMany

lineBlock :: Build Inline a => [a] -> Builder Block
lineBlock = tellOne . B.LineBlock . fmap buildToList

codeBlock :: Text -> Builder Block
codeBlock = codeBlockWith B.nullAttr

codeBlockWith :: B.Attr -> Text -> Builder Block
codeBlockWith attrs = tellOne . B.CodeBlock attrs

rawBlock :: Text -> Text -> Builder Block
rawBlock format = tellOne . B.RawBlock (B.Format format)

blockQuote :: Build Block a => a -> Builder Block
blockQuote = liftWrapper B.BlockQuote

orderedList :: Build Block a => [a] -> Builder Block
orderedList = orderedListWith (1, B.DefaultStyle, B.DefaultDelim)

orderedListWith :: Build Block a => B.ListAttributes -> [a] -> Builder Block
orderedListWith attrs = tellOne . B.OrderedList attrs . fmap buildToList

bulletList :: Build Block a => [a] -> Builder Block
bulletList = tellOne . B.BulletList . fmap buildToList

definitionList :: (Build Inline a, Build Block b) => [(a, [b])] -> Builder Block
definitionList = tellOne . B.DefinitionList . fmap (buildToList *** fmap buildToList)

header :: Build Inline a => Int -> a -> Builder Block
header = headerWith B.nullAttr

headerWith :: Build Inline a => B.Attr -> Int -> a -> Builder Block
headerWith attr level = liftWrapper $ B.Header level attr

horizontalRule :: Builder Block
horizontalRule = tellOne B.HorizontalRule

simpleCell :: Build Block a => a -> B.Cell
simpleCell = cell B.AlignDefault 1 1

cell
  :: Build Block a
  => B.Alignment
  -> B.RowSpan
  -> B.ColSpan
  -> a
  -> B.Cell
cell = cellWith B.nullAttr

cellWith
  :: Build Block a
  => B.Attr
  -> B.Alignment
  -> B.RowSpan
  -> B.ColSpan
  -> a
  -> B.Cell
cellWith attrs align rowspan colspan = B.Cell attrs align rowspan colspan . buildToList

emptyCell :: B.Cell
emptyCell = simpleCell ()

table :: B.Caption
      -> [B.ColSpec]
      -> B.TableHead
      -> [B.TableBody]
      -> B.TableFoot
      -> Builder Block
table = tableWith B.nullAttr

tableWith :: B.Attr
          -> B.Caption
          -> [B.ColSpec]
          -> B.TableHead
          -> [B.TableBody]
          -> B.TableFoot
          -> Builder Block
tableWith = (((((buildMany .) .) .) .) .) . B.tableWith

simpleTable :: (Build Block a, Build Block b) => [a] -> [[b]] -> Builder Block
simpleTable headers rows = buildMany $ B.simpleTable (fmap buildToMany headers) (fmap buildToMany <$> rows)

#if MIN_VERSION_pandoc_types(1,23,0)
figure :: Build Block a => B.Caption -> a -> Builder Block
figure = figureWith B.nullAttr

figureWith :: Build Block a => B.Attr -> B.Caption -> a -> Builder Block
figureWith attr capt = liftWrapper $ B.Figure attr capt
#endif

caption :: Build Block a => Maybe B.ShortCaption -> a -> B.Caption
caption x = B.Caption x . buildToList

simpleCaption :: Build Block a => a -> B.Caption
simpleCaption = caption Nothing

emptyCaption :: B.Caption
emptyCaption = simpleCaption ()

#if MIN_VERSION_pandoc_types(1,22,1)
simpleFigureWith :: Build Inline a => B.Attr -> a -> Text -> Text -> Builder Block
simpleFigureWith attr = ((buildMany .) .) . B.simpleFigureWith attr . buildToMany

simpleFigure :: Build Inline a => a -> Text -> Text -> Builder Block
simpleFigure = simpleFigureWith B.nullAttr
#endif

divWith :: Build Block a => B.Attr -> a -> Builder Block
divWith attr = liftWrapper $ B.Div attr
