module Continuations.Actions (display, ioAction, link, label, startTask, choice) where

import Continuations.Base
import Continuations.Types
import Data.List (intersperse)
import Text.XHtml.Strict hiding (URL, input, form, label)
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F

link :: (HTML a, FromAction f) => a -> f ()
link =  fromAction . Link . toHtml

display :: (HTML a, FromAction f) => a -> f ()
display =  fromAction . Display . toHtml

ioAction :: (FromAction f) => IO a -> f a
ioAction = fromAction . IOAction

choice :: [(String, StartTask)] -> Task ()
choice = Choice choices

choices = X.concatHtml . intersperse X.br . map (\(n, StartTask url _) ->
            X.anchor ! [X.href $ "/" ++ url] << n)

startTask = StartTask

label :: String -> Form a -> Form a
label text = F.plug (\f -> (X.label << text) +++ f)
