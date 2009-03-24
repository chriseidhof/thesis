module Continuations.Actions (display, ioAction, link, label, startTask, choice) where

import Continuations.Base
import Continuations.Types
import Data.List (intersperse)
import Text.XHtml.Strict hiding (URL, input, form, label)
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F

link :: (HTML html, FromAction task) => html -> task ()
link =  fromAction . Link . toHtml

display :: (HTML html, FromAction task) => html -> task ()
display =  fromAction . Display . toHtml

ioAction :: (FromAction task) => IO a -> task a
ioAction = fromAction . IOAction

choice :: [(String, StartTask)] -> Task ()
choice = Choice choices

choices = X.concatHtml . intersperse X.br . map (\(n, StartTask url _) ->
            X.anchor ! [X.href $ "/" ++ url] << n)

startTask = StartTask

label :: String -> Form a -> Form a
label text = F.plug (\f -> (X.label << text) +++ f)
