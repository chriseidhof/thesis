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

gRepInput :: (Representable a b) => Form a
gRepInput = from <$> inp (rep undefined)

inp :: RepT a r -> Form (Rep a r)
inp (RInt)         = undefined
inp (RInteger)     = TInteger <$> F.inputInteger Nothing
inp (RString)      = TString  <$> F.input Nothing
inp (RField lbl v) = Field lbl <$> (F.plug (\x -> (X.label << (capitalize lbl ++ ": ") +++ x)) $ inp v)
inp (RProduct l r) = (:*:) <$> inp l <*> (F.plug (\x -> X.br +++ x) $ inp r)

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
