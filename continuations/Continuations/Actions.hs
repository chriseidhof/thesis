module Continuations.Actions (display, ioAction, label) where

import Continuations.Base
import Continuations.Types
import Data.List (intersperse)
import Text.XHtml.Strict hiding (URL, input, form, label)
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F

display :: (HTML html, FromAction task) => html -> task ()
display html =  fromAction $ Wrapped (const $ X.toHtml html) (Const ())

wrap :: (FromAction task) => (X.Html -> X.Html) -> Action a -> task a
wrap f = fromAction . Wrapped f

ioAction :: (FromAction task) => IO a -> task a
ioAction = fromAction . IOAction

label :: String -> Form a -> Form a
label text = F.plug (\f -> (X.label << text) +++ f)
