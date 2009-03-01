-- The Arc Challenge
module Samples.Arc (arc) where

import Continuations
import Continuations.Types
import Text.XHtml.Strict ((+++))
import qualified Text.XHtml.Strict as X

arc = startTask "arc" $ do 
  name <- input
  link "click here"
  display $ "You said: " ++ name
