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
  view x = repView (rep x) $ to x
   
repView :: Rep r -> r -> X.Html
repView RInt             i = toHtml (show i)
repView RInteger         i = toHtml (show i)
repView RString          s = toHtml s
repView (Field lbl r)    v = (X.label << (capitalize lbl ++ ": ")) +++ repView r v
repView (r1 :*: r2) (l, r) = repView r1 l +++ X.br +++ repView r2 r

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

class TableView a where
   headers :: a -> [X.Html]
   cells   :: a -> [X.Html]

instance (Representable a r) => TableView a where
  headers x = repHeaders (rep x)
  cells   x = repCells (rep x) (to x)

table :: TableView a => [a] -> X.Html
table ls = X.table << ((tr << (concatHtml $ map th $ headers $ head ls)) +++
                       (concatHtml $ map (tr . concatHtml . map td . cells) ls)
                      )

repHeaders :: Rep r -> [X.Html]
repHeaders (Field lbl _) = [toHtml $ capitalize lbl]
repHeaders (l :*: r)     = repHeaders l ++ repHeaders r
repHeaders _             = []

repCells :: Rep r -> r -> [X.Html]
repCells RInt            i = [toHtml $ show i]
repCells RInteger        i = [toHtml $ show i]
repCells RString         s = [toHtml s]
repCells (Field lbl r)   v = [concatHtml (repCells r v)]
repCells (r1 :*: r2) (l,r) = repCells r1 l ++ repCells r2 r
