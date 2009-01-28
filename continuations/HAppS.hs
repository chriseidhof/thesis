module HAppS where

import HAppS.Server
import Continuations
import Control.Concurrent.MVar
import Control.Monad.Trans
import Text.Printf
import qualified Text.XHtml.Strict.Formlets as F
import qualified Data.ByteString.Lazy.Char8 as B
import Continuations.Types
import Continuations.HAppS

main :: IO ()
main = runServer 8016 [("/", const arc)]


arc = do name <- getInput
         link "click here"
         display $ "You said: " ++ name
