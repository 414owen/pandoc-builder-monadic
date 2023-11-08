{-# LANGUAGE FlexibleInstances    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Pandoc.Builder.Monadic.Internal where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.DList                  (DList)
import Data.Foldable               (traverse_)
import Text.Pandoc.Builder         (Inline)

import qualified Text.Pandoc.Builder         as B
import qualified Data.DList                  as DList

type Builder el = Writer (DList el) ()

runToList :: Builder el -> [el]
runToList = DList.toList . execWriter

runToMany :: Builder a -> B.Many a
runToMany = B.fromList . DList.toList . execWriter

type Author = Builder Inline

instance B.ToMetaValue (Builder Inline) where
  toMetaValue = B.MetaInlines . runToList

instance B.ToMetaValue (Builder Author) where
  toMetaValue = B.MetaList . map B.toMetaValue . runToList

buildMany :: B.Many a -> Builder a
buildMany = traverse_ (tell . pure)

tellOne :: a -> Builder a
tellOne = tell . pure
