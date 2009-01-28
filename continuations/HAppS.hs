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
main = runServer 8016 [home, arc, adder]

home = startTask "/" $ do
  choice [("Arc", arc), ("Adder", adder)]

arc = startTask "arc" $ do 
  name <- getInput
  link "click here"
  display $ "You said: " ++ name

adder = startTask "adder" $ do 
  (x,y) <- getInput :: Task (Integer, Integer)
  link "add the two numbers"
  display $ "The sum is : " ++ show (x + y)
