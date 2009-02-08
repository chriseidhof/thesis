module Generics.SimpleInput where

import Generics.Records
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X
import Data.Char (toUpper)
import Continuations.Types
import Control.Applicative
import qualified Text.XHtml.Strict.Formlets as F
import Data.Maybe (fromJust)

gRepInput :: Representable a => Maybe a -> Form a
gRepInput x = from <$> inp (rep $ fromJust x) (to <$> x)

inp :: Rep r -> Maybe r -> Form r
inp RInt          x = fromIntegral <$> F.inputInteger (fromIntegral <$> x)
inp RInteger      x = F.inputInteger x
inp RString       x = F.input x
inp (Field lbl v) x = (F.plug (\x -> (X.label << (capitalize lbl ++ ": ") +++ x)) $ inp v x)
inp (l :*: r)     x = (,) <$> inp l (fst <$> x) <*> (F.plug (\x -> X.br +++ x) $ inp r (snd <$> x))

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
