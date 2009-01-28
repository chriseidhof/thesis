module HAppS where

import HAppS.Server hiding (simpleInput)
import Control.Applicative
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

data User = User {name :: String, age :: Integer, email :: String}

instance SimpleInput User where
  simpleInput = User <$> simpleInput <*> simpleInput <*> simpleInput

home = startTask "/" $ do
  choice [("Arc", arc), ("Adder", adder)]

arc = startTask "arc" $ do 
  name <- input
  link "click here"
  display $ "You said: " ++ name

adder = startTask "adder" $ do 
  display "Please register first."
  user <- register
  (x,y) <- input :: Task (Integer, Integer)
  link "add the two numbers"
  display $ "The sum is : " ++ show (x + y)
  display $ "Thanks, " ++ (name user)

register :: Task User
register = input
