{-# LANGUAGE FlexibleInstances #-}

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

newtype BuilderM el a = Builder { unBuilder :: Writer (DList el) a }

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

type Builder el = BuilderM el ()

type Author = Builder Inline

instance B.ToMetaValue (Builder Inline) where
  toMetaValue = B.MetaInlines . runToList

instance B.ToMetaValue (Builder Author) where
  toMetaValue = B.MetaList . map B.toMetaValue . runToList

runToDList :: Builder el -> DList el
runToDList = execWriter . unBuilder

runToList :: Builder el -> [el]
runToList = DList.toList . runToDList

runToMany :: Builder a -> B.Many a
runToMany = B.fromList . DList.toList . execWriter . unBuilder

buildMany :: B.Many a -> Builder a
buildMany = Builder . traverse_ (tell . pure)

tellOne :: a -> Builder a
tellOne = Builder . tell . pure

tellAll :: DList a -> Builder a
tellAll = Builder . tell
