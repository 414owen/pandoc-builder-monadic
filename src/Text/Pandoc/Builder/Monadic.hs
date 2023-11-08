{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Pandoc.Builder.Monadic
  ( doc
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
  , h1
  , h2
  , h3
  , h4
  , h5
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
import Control.Monad.Writer.Strict (Writer)
import Data.DList                  (DList)
import Data.Text                   (Text)

import Text.Pandoc.Builder (Inline, Block, Pandoc)
import Data.String (IsString(..))

import Text.Pandoc.Builder.Monadic.Internal
  ( buildMany
  , runToMany
  , runToList
  , tellOne
  )

import qualified Data.Text           as Text
import qualified Text.Pandoc.Builder as B

type Builder el = Writer (DList el) ()

instance IsString (Builder Inline) where
  fromString = str . Text.pack

instance IsString (Builder Block) where
  fromString = plain . str . Text.pack

doc :: Builder Block -> Pandoc
doc = B.Pandoc mempty . runToList

text :: Text -> Builder Inline
text = buildMany . B.text

str :: Text -> Builder Inline
str = tellOne . B.Str

liftWrapper :: ([a] -> b) -> Builder a -> Builder b
liftWrapper f = tellOne . f . runToList

emph :: Builder Inline -> Builder Inline
emph = liftWrapper B.Emph

underline :: Builder Inline -> Builder Inline
underline = liftWrapper B.Underline

strong :: Builder Inline -> Builder Inline
strong = liftWrapper B.Strong

strikeout :: Builder Inline -> Builder Inline
strikeout = liftWrapper B.Strikeout

superscript :: Builder Inline -> Builder Inline
superscript = liftWrapper B.Superscript

subscript :: Builder Inline -> Builder Inline
subscript = liftWrapper B.Subscript

smallcaps :: Builder Inline -> Builder Inline
smallcaps = liftWrapper B.SmallCaps

singleQuoted :: Builder Inline -> Builder Inline
singleQuoted = liftWrapper $ B.Quoted B.SingleQuote

doubleQuoted :: Builder Inline -> Builder Inline
doubleQuoted = liftWrapper $ B.Quoted B.DoubleQuote

cite :: [B.Citation] -> Builder Inline -> Builder Inline
cite citations = tellOne . B.Cite citations . runToList

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

link :: Text -> Text -> Builder Inline -> Builder Inline
link = linkWith B.nullAttr

linkWith :: B.Attr -> Text -> Text -> Builder Inline -> Builder Inline
linkWith attr url title x = tellOne $ B.Link attr (runToList x) (url, title)

image :: Text -> Text -> Builder Inline -> Builder Inline
image = imageWith B.nullAttr

imageWith :: B.Attr -> Text -> Text -> Builder Inline -> Builder Inline
imageWith attr url title x = tellOne $ B.Image attr (runToList x) (url, title)

note :: Builder B.Block -> Builder Inline
note = tellOne . B.Note . runToList

spanWith :: B.Attr -> Builder Inline -> Builder Inline
spanWith attr = tellOne . B.Span attr . runToList

trimInlines :: Builder Inline -> Builder Inline
trimInlines = buildMany . B.trimInlines . runToMany

-- Block list builders

para :: Builder Inline -> Builder Block
para = liftWrapper B.Para

plain :: Builder Inline -> Builder Block
plain = buildMany . B.plain . runToMany

lineBlock :: [Builder Inline] -> Builder Block
lineBlock = tellOne . B.LineBlock . fmap runToList

codeBlock :: Text -> Builder Block
codeBlock = codeBlockWith B.nullAttr

codeBlockWith :: B.Attr -> Text -> Builder Block
codeBlockWith attrs = tellOne . B.CodeBlock attrs

rawBlock :: Text -> Text -> Builder Block
rawBlock format = tellOne . B.RawBlock (B.Format format)

blockQuote :: Builder Block -> Builder Block
blockQuote = tellOne . B.BlockQuote . runToList

orderedList :: [Builder Block] -> Builder Block
orderedList = orderedListWith (1, B.DefaultStyle, B.DefaultDelim)

orderedListWith :: B.ListAttributes -> [Builder Block] -> Builder Block
orderedListWith attrs = tellOne . B.OrderedList attrs . fmap runToList

bulletList :: [Builder Block] -> Builder Block
bulletList = tellOne . B.BulletList . fmap runToList

definitionList :: [(Builder Inline, [Builder Block])] -> Builder Block
definitionList = tellOne . B.DefinitionList . fmap (runToList *** fmap runToList)

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

header :: Int -> Builder Inline -> Builder Block
header = headerWith B.nullAttr

headerWith :: B.Attr -> Int -> Builder Inline -> Builder Block
headerWith attr level = tellOne . B.Header level attr . runToList

horizontalRule :: Builder Block
horizontalRule = tellOne B.HorizontalRule

simpleCell :: Builder Block -> B.Cell
simpleCell = cell B.AlignDefault 1 1

cell
  :: B.Alignment
  -> B.RowSpan
  -> B.ColSpan
  -> Builder Block
  -> B.Cell
cell = cellWith B.nullAttr

cellWith
  :: B.Attr
  -> B.Alignment
  -> B.RowSpan
  -> B.ColSpan
  -> Builder Block
  -> B.Cell
cellWith attrs align rowspan colspan = B.Cell attrs align rowspan colspan . runToList

emptyCell :: B.Cell
emptyCell = simpleCell $ pure ()

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

simpleTable :: [Builder Block] -> [[Builder Block]] -> Builder Block
simpleTable headers rows = buildMany $ B.simpleTable (fmap runToMany headers) (fmap runToMany <$> rows)

#if MIN_VERSION_pandoc_types(1,23,0)
figure :: B.Caption -> Builder Block -> Builder Block
figure = figureWith B.nullAttr

figureWith :: B.Attr -> B.Caption -> Builder Block -> Builder Block
figureWith attr capt = tellOne . B.Figure attr capt . runToList
#endif

caption :: Maybe B.ShortCaption -> Builder Block -> B.Caption
caption x = B.Caption x . runToList

simpleCaption :: Builder Block -> B.Caption
simpleCaption = caption Nothing

emptyCaption :: B.Caption
emptyCaption = simpleCaption $ pure ()

#if MIN_VERSION_pandoc_types(1,22,1)
simpleFigureWith :: B.Attr -> Builder Inline -> Text -> Text -> Builder Block
simpleFigureWith attr = ((buildMany .) .) . B.simpleFigureWith attr . runToMany

simpleFigure :: Builder Inline -> Text -> Text -> Builder Block
simpleFigure = simpleFigureWith B.nullAttr
#endif

divWith :: B.Attr -> Builder Block -> Builder Block
divWith attr = tellOne . B.Div attr . runToList
