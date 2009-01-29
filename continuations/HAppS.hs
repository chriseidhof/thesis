{-# LANGUAGE DeriveDataTypeable #-}
module HAppS where

import Control.Applicative
import Continuations
import Control.Concurrent.MVar
import Control.Monad.Trans
import Text.Printf
import qualified Text.XHtml.Strict.Formlets as F
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((+++))
import Continuations.Types
import Continuations.Salvia

main :: IO ()
main = runServer 8016 [home, arc, adder]


data User a = User {name :: String, age :: Integer, email :: String}

instance SimpleInput (User a) where
  simpleInput = User <$> label "Name:" simpleInput <*> label "Age:" simpleInput <*> label "Email:" simpleInput

home = startTask "/" $ do
  choice [("Arc", arc), ("Adder", adder)]

arc = startTask "arc" $ do 
  name <- input
  wrap (\h -> "Click on the link to continue" +++ X.br +++ h) $ link "click here"
  display $ "You said: " ++ name

adder = startTask "adder" $ do 
  display "Please register first."
  user <- register
  (x,y) <- input :: Task (Integer, Integer)
  link "add the two numbers"
  display $ "The sum is : " ++ show (x + y)
  display $ "Thanks, " ++ (name user)

register = do u <- wrap (\h -> "Give your user details" +++ h) input
              display "Thanks for registering"
              return u
