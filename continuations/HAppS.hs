{-# LANGUAGE DeriveDataTypeable #-}
module HAppS where

import Continuations
import Continuations.Salvia
import Continuations.Types
import Database.HDBC.PostgreSQL
import Generics.Views
import Text.XHtml.Strict ((+++))
import qualified Text.XHtml.Strict as X

import Model
import Samples.Arc
import Samples.User

main :: IO ()
main = do conn <- connectPostgreSQL "port=5432 user=chris dbname=test"
          runServer 8016 [home conn, arc, adder conn, xmlTask, findUser conn]

home conn = startTask "/" $ do
  choice [("Arc", arc), ("Adder", adder conn), ("Xml", xmlTask), ("Find User", findUser conn)]

adder conn = startTask "adder" $ do 
  display "Please register first."
  user <- register
  userId <- gNew conn user
  display $ "Your user id is: " ++ show userId
  (x,y) <- input :: Task (Integer, Integer)
  link "add the two numbers"
  display $ "The sum is : " ++ show (x + y)
  wrap (("Thanks, " +++ X.br) +++) $ display (view user)

