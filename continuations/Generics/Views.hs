{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Views where

import Generics.Records
import Text.XHtml.Strict ((+++))
import Text.XHtml.Strict as X
import Data.Char (toUpper)

class View a where
  view :: a -> X.Html

instance (Representable a b) => View a where
  view = repView . to
   
repView :: Rep a r -> X.Html
repView (TInt i)      = toHtml (show i)
repView (TInteger i)  = toHtml (show i)
repView (TString s)   = toHtml s
repView (Field lbl v) = (X.label << (capitalize lbl ++ ": ")) +++ repView v
repView (l :*: r)     = repView l +++ X.br +++ repView r

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

class TableView a where
   headers :: a -> [X.Html]
   cells   :: a -> [X.Html]

instance (Representable a r) => TableView a where
  headers x = repHeaders (rep x)
  cells   x = repCells   (to x)

table :: TableView a => [a] -> X.Html
table ls = X.table << ((tr << (concatHtml $ map th $ headers $ head ls)) +++
                       (concatHtml $ map (tr . concatHtml . map td . cells) ls)
                      )

repHeaders :: RepT a r -> [X.Html]
repHeaders (RField lbl _) = [toHtml $ capitalize lbl]
repHeaders (RProduct l r) = repHeaders l ++ repHeaders r
repHeaders _              = []

repCells :: Rep a r -> [X.Html]
repCells (TInt i)      = [toHtml $ show i]
repCells (TInteger i)  = [toHtml $ show i]
repCells (TString s)   = [toHtml s]
repCells (Field lbl v) = [concatHtml (repCells v)]
repCells (l :*: r)     = repCells l ++ repCells r
