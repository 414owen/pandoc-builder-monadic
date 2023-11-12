{-# LANGUAGE FlexibleInstances #-}

{-|
Internal module, exposing the Builder(..) type, you should
prefer using 'Text.Pandoc.Builder.Monadic.Utils.mapBuilder',
or adding something to 'Text.Pandoc.Builder.Monadic.Utils'.
-}

module Text.Pandoc.Builder.Monadic.Internal
  ( BuilderM(..)
  , Builder
  , buildMany
  , runToList
  , runToDList
  , runToMany
  , tellOne
  , tellAll
  ) where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.DList                  (DList)
import Data.Foldable               (traverse_)
import Text.Pandoc.Builder         (Inline)

import qualified Text.Pandoc.Builder         as B
import qualified Data.DList                  as DList

-- | The pandoc element builder type. It wraps a writer monad.
-- Chances are, you only need t'Builder' (a 'BuilderM el ()').
-- All builders in this library have an `el` type in the set
-- {'B.Inline', 'B.Block'}.
newtype BuilderM el a = Builder { unBuilder :: Writer (DList el) a }

-- | Pandoc element builder. Stores written pandoc elements.
type Builder el = BuilderM el ()

instance Functor (BuilderM el) where
  fmap f = Builder . fmap f . unBuilder

instance Applicative (BuilderM el) where
  pure a = Builder $ pure a
  Builder f <*> Builder a = Builder $ f <*> a

instance Monad (BuilderM el) where
  Builder a >>= f = Builder $ do
    a' <- a
    unBuilder $ f a'

instance Semigroup (BuilderM el a) where
  Builder a <> Builder b = Builder $ a >> b

instance Monoid a => Monoid (BuilderM el a) where
  mempty = Builder $ pure mempty

instance B.ToMetaValue (Builder Inline) where
  toMetaValue = B.MetaInlines . runToList

-- | Useful for setting authors
instance B.ToMetaValue (Builder (Builder Inline)) where
  toMetaValue = B.MetaList . map B.toMetaValue . runToList

-- | Get elements written in the t'Builder' as a difference list
runToDList :: Builder el -> DList el
runToDList = execWriter . unBuilder

-- | Get elements written in the t'Builder' as a list
runToList :: Builder el -> [el]
runToList = DList.toList . runToDList

-- | Get elements written in a t'Builder' as a 'B.Many'.
-- This might be useful if you need to interact with pandoc-types.
runToMany :: Builder a -> B.Many a
runToMany = B.fromList . DList.toList . execWriter . unBuilder

-- | Get pandoc-types' 'B.Many' as a t'Builder'.
buildMany :: B.Many a -> Builder a
buildMany = Builder . traverse_ (tell . pure)

-- | Write a single element to a t'Builder'.
tellOne :: a -> Builder a
tellOne = Builder . tell . DList.singleton

-- | Write multiple element to a t'Builder'.
tellAll :: DList a -> Builder a
tellAll = Builder . tell
