-- The Arc Challenge
module Samples.Arc (arc) where

import Continuations
import Continuations.Types
import Text.XHtml.Strict ((+++))
import qualified Text.XHtml.Strict as X

arc = startTask "arc" $ do 
  name <- input
  wrap (\h -> "Click on the link to continue" +++ X.br +++ h) $ link "click here"
  display $ "You said: " ++ name
