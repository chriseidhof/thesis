{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Xml where

import Generics.Records
import qualified Text.XML.Light as X

class Xml a where
  xml :: a -> X.Element

instance (Representable a) => Xml a where
  xml a = case repXml (rep a) (to a) of
               [X.Elem e] -> e
               x          -> el "item" x
   
repXml :: Rep r -> r -> [X.Content]
repXml (RInt)      i = [showXml i]
repXml (RInteger)  i = [showXml i]
repXml (RString)   s = [X.Text $ X.CData X.CDataText s Nothing]
repXml (Field lbl r) i = [simple lbl (repXml r i)]
repXml (r1 :*: r2) (l,r) = repXml r1 l ++ repXml r2 r
repXml (Con lbl r) x = [simple lbl (repXml r x)]

showXml :: Show s => s -> X.Content
showXml x = X.Text $ X.CData X.CDataText (show x) Nothing

simple n c = X.Elem $ el n c
el n c = X.Element (X.QName n Nothing Nothing) [] c Nothing
