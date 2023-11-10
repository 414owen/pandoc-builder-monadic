{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Builder.Monadic.Internal
  ( Builder
  , Build(..)
  , buildMany
  , runToList
  , runToMany
  , tellOne
  ) where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.DList                  (DList)
import Data.Foldable               (traverse_)
import Data.Text                   (Text)
import Text.Pandoc.Builder         (Inline)

import qualified Data.DList                  as DList
import qualified Text.Pandoc.Builder         as B
import qualified Data.Text                   as T

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

runToList :: Builder el -> [el]
runToList = DList.toList . execWriter . unBuilder

runToMany :: Builder a -> B.Many a
runToMany = B.fromList . DList.toList . execWriter . unBuilder

type Author = Builder Inline

instance B.ToMetaValue (Builder Inline) where
  toMetaValue = B.MetaInlines . runToList

instance B.ToMetaValue (Builder Author) where
  toMetaValue = B.MetaList . map B.toMetaValue . runToList

tellOne :: a -> Builder a
tellOne = Builder . tell . pure


buildMany :: B.Many a -> Builder a
buildMany = Builder . traverse_ (tell . pure)

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
  buildToMany _ = buildToMany ([] :: [el])

instance Build Inline Text where
  buildToList s = [B.Str s]

instance Build Inline String where
  buildToList = buildToList . T.pack
  buildToMany = buildToMany . T.pack
