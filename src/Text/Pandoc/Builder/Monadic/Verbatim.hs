{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

-- We define some instances for Builder here, because this is where we
-- have functions to implement them.
{-# OPTIONS_GHC -Wno-orphans #-}

-- We use named parameters as a form of documentation.
{- HLINT ignore "Eta reduce" -}

-- | This module exports a 1:1 monadic version of pandoc-types' 'Text.Pandoc.Builder'.

module Text.Pandoc.Builder.Monadic.Verbatim
  ( module Text.Pandoc.Definition
  , Builder
  , URL
  , Title
  , Raw

  -- * Top-level
  , doc
  , setTitle
  , setAuthors
  , setDate
  , setMeta

  -- * Inline builders
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
  , code
  , codeWith
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

  -- * Block builders
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
import Data.String (IsString(..))

import Text.Pandoc.Builder.Monadic.Internal
  ( Builder
  , buildMany
  , runToMany
  , runToList
  , tellOne
  )

import qualified Data.Text           as Text
import qualified Text.Pandoc.Builder as B

instance IsString (Builder Inline) where
  fromString = str . Text.pack

instance IsString (Builder Block) where
  fromString = plain . str . Text.pack

-- | Lifts something (usually a Pandoc data constructor), into
-- a builder which takes a builder.
liftWrapper :: ([a] -> b) -> Builder a -> Builder b
liftWrapper f = tellOne . f . runToList

liftWrapper' :: (B.Many a -> B.Many b) -> Builder a -> Builder b
liftWrapper' f = buildMany . f . runToMany

-- | Build a pandoc document from a 'Builder' of top-level elements.
doc :: Builder Block -> Pandoc
doc = B.Pandoc mempty . runToList

-- | Set the document's title in the metadata.
setTitle :: Builder Inline -> Pandoc -> Pandoc
setTitle = B.setTitle . runToMany

-- | Set the document's authors in the metadata.
setAuthors :: [Builder Inline] -> Pandoc -> Pandoc
setAuthors = B.setAuthors . fmap runToMany

-- | Set the document's date in the metadata.
setDate :: Builder Inline -> Pandoc -> Pandoc
setDate = B.setDate . runToMany

-- | Set a value in the document's metadata.
setMeta :: (B.HasMeta a, B.ToMetaValue b) => Text -> b -> a -> a 
setMeta = B.setMeta

-- | Convert a 'Text' to a 'Builder' 'Inline', treating interword spaces as 'B.Space's
-- or 'B.SoftBreak's. If you want a 'B.Str' with literal spaces, use 'str'.
text :: Text -> Builder Inline
text = buildMany . B.text

-- | Build a string.
str :: Text -> Builder Inline
str = tellOne . B.Str

-- | Build an emphasized (usually italicized) inline.
emph :: Builder Inline -> Builder Inline
emph = liftWrapper B.Emph

-- | Build an underlined inline.
underline :: Builder Inline -> Builder Inline
underline = liftWrapper B.Underline

-- | Build a strong (bold) inline.
strong :: Builder Inline -> Builder Inline
strong = liftWrapper B.Strong

-- | Build a strikeout (crossed out) inline.
strikeout :: Builder Inline -> Builder Inline
strikeout = liftWrapper B.Strikeout

-- | Build a superscripted inline.
superscript :: Builder Inline -> Builder Inline
superscript = liftWrapper B.Superscript

-- | Build a subscripted inline.
subscript :: Builder Inline -> Builder Inline
subscript = liftWrapper B.Subscript

-- | Build a smallcaps inline. See the example in the font-family [MDN page](https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant#examples).
smallcaps :: Builder Inline -> Builder Inline
smallcaps = liftWrapper B.SmallCaps

-- | Build a single-quoted inline.
singleQuoted :: Builder Inline -> Builder Inline
singleQuoted = liftWrapper $ B.Quoted B.SingleQuote

-- | Build a double-quoted inline.
doubleQuoted :: Builder Inline -> Builder Inline
doubleQuoted = liftWrapper $ B.Quoted B.DoubleQuote

-- | Build a citation. See
-- [Citations in note style](https://pandoc.org/chunkedhtml-demo/9.3-citations-in-note-styles.html)
-- and [Specifying a citation style](https://pandoc.org/chunkedhtml-demo/9.2-specifying-a-citation-style.html).
cite :: [B.Citation] -> Builder Inline -> Builder Inline
cite citations = liftWrapper $ B.Cite citations

-- | Build some inline code.
code :: Text -> Builder Inline
code = codeWith B.nullAttr

-- | Build some inline code with attributes.
codeWith :: B.Attr -> Text -> Builder Inline
codeWith = (tellOne .) . B.Code

-- | Build an inter-word space.
space :: Builder Inline
space = tellOne B.Space

-- | Build a soft line-break.
softbreak :: Builder Inline
softbreak = tellOne B.SoftBreak

-- | Build a hard line-break.
linebreak :: Builder Inline
linebreak = tellOne B.LineBreak

-- | Build some inline TeX math.
math :: Text -> Builder Inline
math = tellOne . B.Math B.InlineMath

-- | Build some display-mode TeX math.
-- Display mode is for math that is set apart from the main text.
displayMath :: Text -> Builder Inline
displayMath = tellOne . B.Math B.DisplayMath

type Raw = Text

-- | Embed some of the output directly.
-- This is useful to gain access to, features of the underlying
-- output not supported directly by pandoc.
rawInline :: Format -> Raw -> Builder Inline
rawInline format = tellOne . B.RawInline format

type URL = Text
type Title = Text

-- | Build a link from a URL, a title, and some inline pandoc.
link :: URL -> Title -> Builder Inline -> Builder Inline
link url title = linkWith B.nullAttr url title

-- | Build a link from some attributes, a URL, a title, and some inline pandoc.
linkWith :: B.Attr -> URL -> Title -> Builder Inline -> Builder Inline
linkWith attr url title x = tellOne $ B.Link attr (runToList x) (url, title)

-- | Build an image from a URL, a title, and some inline pandoc.
image :: URL -> Title -> Builder Inline -> Builder Inline
image url title = imageWith B.nullAttr url title

-- | Build an image from some attributes, a URL, a title, and some inline pandoc.
imageWith :: B.Attr -> Text -> Text -> Builder Inline -> Builder Inline
imageWith attr url title x = tellOne $ B.Image attr (runToList x) (url, title)

-- | Build a footnote or endnote from some pandoc blocks.
note :: Builder B.Block -> Builder Inline
note = liftWrapper B.Note

-- | Build a generic inline container from attributes and more inline pandoc.
spanWith :: B.Attr -> Builder Inline -> Builder Inline
spanWith attr = liftWrapper $ B.Span attr

-- | Trim leading and trailing spaces and softbreaks from some inline pandoc.
trimInlines :: Builder Inline -> Builder Inline
trimInlines = liftWrapper' B.trimInlines

-- Block list builders

-- | Build a paragraph.
para :: Builder Inline -> Builder Block
para = liftWrapper B.Para

-- | Build some plain text (not a paragraph).
plain :: Builder Inline -> Builder Block
plain = liftWrapper' B.plain

-- | Build multiple non-breaking lines.
lineBlock :: [Builder Inline] -> Builder Block
lineBlock = tellOne . B.LineBlock . fmap runToList

-- | Build a code block.
codeBlock :: Text -> Builder Block
codeBlock = codeBlockWith B.nullAttr

-- | Build a code block with attributes.
codeBlockWith :: B.Attr -> Text -> Builder Block
codeBlockWith attrs = tellOne . B.CodeBlock attrs

-- | Embed some of the output directly.
-- This is useful to gain access to, features of the underlying
-- output not supported directly by pandoc.
rawBlock :: Format -> Raw -> Builder Block
rawBlock format = tellOne . B.RawBlock format

-- | Build a block quote.
blockQuote :: Builder Block -> Builder Block
blockQuote = liftWrapper B.BlockQuote

-- | Build an ordered list.
orderedList :: [Builder Block] -> Builder Block
orderedList = orderedListWith (1, B.DefaultStyle, B.DefaultDelim)

-- | Build an ordered list with attributes.
orderedListWith :: B.ListAttributes -> [Builder Block] -> Builder Block
orderedListWith attrs = tellOne . B.OrderedList attrs . fmap runToList

-- | Build a bullet list.
bulletList :: [Builder Block] -> Builder Block
bulletList = tellOne . B.BulletList . fmap runToList

-- | Build an definition list given a list of tuples, where the first element
-- of each tuple is a term, and the second element is the definition.
definitionList :: [(Builder Inline, [Builder Block])] -> Builder Block
definitionList = tellOne . B.DefinitionList . fmap (runToList *** fmap runToList)

-- | Build a header, given a level and some inline pandoc.
-- You may consider using 'Text.Pandoc.Builder.Monadic.h1' and friends,
-- for a more concise API.
header :: Int -> Builder Inline -> Builder Block
header level x = headerWith B.nullAttr level x

-- | Build a header from some attributes, a level and some inline pandoc.
headerWith :: B.Attr -> Int -> Builder Inline -> Builder Block
headerWith attr level = liftWrapper $ B.Header level attr

-- | Build a horizontal rule.
horizontalRule :: Builder Block
horizontalRule = tellOne B.HorizontalRule

-- | Build a 1x1 cell with default alignment, given some pandoc.
simpleCell :: Builder Block -> B.Cell
simpleCell = cell B.AlignDefault 1 1

-- | Build a cell of a table, full API excluding attributes.
cell
  :: B.Alignment
  -> B.RowSpan
  -> B.ColSpan
  -> Builder Block
  -> B.Cell
cell = cellWith B.nullAttr

-- | Build a cell of a table, full API including attributes.
cellWith
  :: B.Attr
  -> B.Alignment
  -> B.RowSpan
  -> B.ColSpan
  -> Builder Block
  -> B.Cell
cellWith attrs align rowspan colspan = B.Cell attrs align rowspan colspan . runToList

-- | Build a 1x1 empty cell.
emptyCell :: B.Cell
emptyCell = simpleCell $ pure ()

-- | Build a table, full API excluding attributes.
table :: B.Caption
      -> [B.ColSpec]
      -> B.TableHead
      -> [B.TableBody]
      -> B.TableFoot
      -> Builder Block
table = tableWith B.nullAttr

-- | Build a table, full API including attributes.
tableWith :: B.Attr
          -> B.Caption
          -> [B.ColSpec]
          -> B.TableHead
          -> [B.TableBody]
          -> B.TableFoot
          -> Builder Block
tableWith = (((((buildMany .) .) .) .) .) . B.tableWith

-- | Build a table, given a list of header cells, and a list of rows.
simpleTable :: [Builder Block] -> [[Builder Block]] -> Builder Block
simpleTable headers rows = buildMany $ B.simpleTable (fmap runToMany headers) (fmap runToMany <$> rows)

#if MIN_VERSION_pandoc_types(1,23,0)
-- | Build a captioned figure.
-- This is available in pandoc-types >= 1.23, which corresponds to pandoc >= 3.0.
figure :: B.Caption -> Builder Block -> Builder Block
figure = figureWith B.nullAttr

-- | Build a captioned figure, with attributes.
-- This is available in pandoc-types >= 1.23, which corresponds to pandoc >= 3.0.
figureWith :: B.Attr -> B.Caption -> Builder Block -> Builder Block
figureWith attr capt = liftWrapper $ B.Figure attr capt
#endif

-- | Make a caption, with an optional short version.
caption :: Maybe B.ShortCaption -> Builder Block -> B.Caption
caption x = B.Caption x . runToList

-- | Make a caption, without a short version.
simpleCaption :: Builder Block -> B.Caption
simpleCaption = caption Nothing

-- | Make an empty caption
emptyCaption :: B.Caption
emptyCaption = simpleCaption mempty

#if MIN_VERSION_pandoc_types(1,22,1)
-- | Build a captioned figure, containing an image.
-- This is available in pandoc-types >= 1.22.1, which corresponds to pandoc >= 2.15.
simpleFigure :: Builder Inline -> Text -> Text -> Builder Block
simpleFigure figureCaption url title = simpleFigureWith B.nullAttr figureCaption url title

-- | Build a captioned figure containing an image, with attributes.
-- This is available in pandoc-types >= 1.22.1, which corresponds to pandoc >= 2.15.
simpleFigureWith :: B.Attr -> Builder Inline -> URL -> Title-> Builder Block
simpleFigureWith attr figureCaption url title
  = buildMany $ B.simpleFigureWith attr (runToMany figureCaption) url title
#endif

-- | Build a generic block container with attributes.
divWith :: B.Attr -> Builder Block -> Builder Block
divWith attr = liftWrapper $ B.Div attr
