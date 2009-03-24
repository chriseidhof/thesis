{-# LANGUAGE DeriveDataTypeable #-}
module Sample where

import Continuations
import Continuations.Salvia
import Continuations.Types
import Database.HDBC.Sqlite3
import Database.HDBC (commit)
import Control.Concurrent (forkIO)
import Generics.Views
import Text.XHtml.Strict ((+++))
import qualified Text.XHtml.Strict as X

import Model
import Samples.Arc
import Samples.User

main :: IO ()
main = do conn <- connectSqlite3 "sample.sqlite3"
          runServer 8016 [home conn, arc, adder conn, xmlTask, findUser conn]
          commit conn

home conn = startTask "/" $ do
  choice [("Arc", arc), ("Adder", adder conn), ("Xml", xmlTask), ("Find User", findUser conn)]

adder conn = startTask "adder" $ do 
  display "Please register first."
  user <- register
  display $ show user
  -- userId <- gNew conn user
  -- display $ "Your user id is: " ++ show userId
  -- (x,(y, z)) <- input :: Task (Integer, (Integer, Integer))
  -- link "add the two numbers"
  -- display $ "The sum is : " ++ show (x + y)
  -- wrap (("Thanks, " +++ X.br) +++) $ display (view user)

