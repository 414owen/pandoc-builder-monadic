# pandoc-builder-monadic


[![CI status badge](https://img.shields.io/github/actions/workflow/status/414owen/pandoc-builder-monadic/haskell-ci.yml)](https://github.com/414owen/pandoc-builder-monadic/actions/workflows/haskell-ci.yml) [![license](https://img.shields.io/github/license/414owen/pandoc-builder-monadic)](https://github.com/414owen/pandoc-builder-monadic/blob/master/LICENSE)

This library provides a monadic DSL for constructing Pandoc documents.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.Builder.Monadic

myDoc :: Pandoc
myDoc = doc $ do
  h1 "Hello, World!"
  para $ do
    str "Lorem ipsum "
    () <- "dolor sit amet"
    traverse (str . T.pack . show) [1..10 :: Int]
    pure ()
  para $ do
    strong "Wow, such code!"
    softbreak
    "It's a " <> strong "monoid" <> " too" <> emph "'cos why not"
```
