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
import Generics.Views

import Model

main :: IO ()
main = runServer 8016 [home, arc, adder, tableTask]

home = startTask "/" $ do
  choice [("Arc", arc), ("Adder", adder), ("Tables", tableTask)]

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
  wrap (\h -> "Thanks, " +++ X.br +++ h) $ display (view user)

tableTask = startTask "table" $ do
  display "Register two users and display them in a table"
  user1 <- register
  user2 <- register
  display (table [user1, user2])

register :: Task User
register = do u <- wrap (\h -> "Give your user details" +++ h) gInput
              display "Thanks for registering"
              return u
