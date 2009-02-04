{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.SimpleInput where

import Generics.Records
import Text.XHtml.Strict ((+++))
import Text.XHtml.Strict as X
import Data.Char (toUpper)
import Continuations.Types
import Control.Applicative
import qualified Text.XHtml.Strict.Formlets as F
import Data.Maybe (fromJust)

gRepInput :: (Representable a b) => Maybe a -> Form a
gRepInput x = from <$> inp (rep $ fromJust x)

inp :: Rep r -> Form r
inp RInt          = fromIntegral <$> F.inputInteger Nothing
inp RInteger      = F.inputInteger Nothing
inp RString       = F.input Nothing
inp (Field lbl v) = (F.plug (\x -> (X.label << (capitalize lbl ++ ": ") +++ x)) $ inp v)
inp (l :*: r)     = (,) <$> inp l <*> (F.plug (\x -> X.br +++ x) $ inp r)

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
