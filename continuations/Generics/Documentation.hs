{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Documentation where

import Generics.Records
import Text.XHtml.Strict ((+++))
import Text.XHtml.Strict as X
import Data.Char (toUpper)
import Data.Maybe (fromJust)

class Documentation a where
  documentation :: a -> X.Html

instance (Representable a) => Documentation a where
  documentation x = repDoc (rep x)
   
repDoc :: Rep r -> X.Html
repDoc RInt          = toHtml "Int"
repDoc RInteger      = toHtml "Integer"
repDoc RString       = toHtml "String"
repDoc (Field lbl r) = (X.label << (capitalize lbl ++ ": ")) +++ repDoc r
repDoc (r1 :*: r2)   = repDoc r1 +++ X.br +++ repDoc r2

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
