{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Xml where

import Generics.Records
import qualified Text.XML.Light as X

class Xml a where
  xml :: a -> X.Element

instance (Representable a b) => Xml a where
  xml = el "item" . repXml . to
   
repXml :: Rep a r -> [X.Content]
repXml (TInt i)      = [showXml i]
repXml (TInteger i)  = [showXml i]
repXml (TString s)   = [X.Text $ X.CData X.CDataText s Nothing]
repXml (Field lbl v) = [simple lbl (repXml v)]
repXml (l :*: r)     = repXml l ++ repXml r

showXml :: Show s => s -> X.Content
showXml x = X.Text $ X.CData X.CDataText (show x) Nothing

simple n c = X.Elem $ el n c
el n c = X.Element (X.QName n Nothing Nothing) [] c Nothing
