# pandoc-builder-monadic

This module provides a monadic DSL for constructing Pandoc documents.


```haskell
import Text.Pandoc.Builder.Monadic

myDoc :: Pandoc
myDoc = doc $ do
  h1 "Work log"
  para $ do
    kv "month" "2023-10"
    emph $ str k
    str ": "
    linebreak
  para $ do
    code "Wow, such code!"
    code "It's a monoid too" <> strong "'cos why not"
```
