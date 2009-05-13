{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Sample where

import Continuations
import Continuations.Salvia
import Continuations.Types
import Database.HDBC (commit)
import Database.HDBC.Sqlite3
import Generics.Records (Rep)
import Generics.Records.ModelName (ModelName)
import Text.XHtml.Strict ((+++))
import Text.XHtml.Strict.Formlets (enumSelect)
import qualified Generics.Records.Database as DB
import qualified Generics.Records.Forms    as GF
import qualified Text.XHtml.Strict as X

main :: IO ()
main = runServer 8016 [sample]

data CRU = Create | List
 deriving (Show, Read, Eq, Enum, Bounded)

sample =     (form $ enumSelect Nothing)
     `Edge` (Choice (\x -> x == Create) create list)

create = undefined -- (form (GF.form Nothing)) `Edge` dbNew
list = undefined

dbNew :: (Rep DB.Values a, Rep DB.Columns a, Rep ModelName a, Show a, Read a) => a :-> Int
dbNew = dbAction DB.new

dbAction :: (Show a, Read a) => (a -> DB.DB b) -> a :-> b
dbAction a = Action (IOAction . doIt)
 where doIt x = do conn <- connectSqlite3 "sample.sqlite3"
                   DB.runDB conn (a x)

-- main :: IO ()
-- main = do conn <- connectSqlite3 "sample.sqlite3"
--           runServer 8016 [home conn, arc, adder conn, xmlTask, findUser conn]
--           commit conn

-- home conn = startTask "/" $ do
--   choice [("Arc", arc), ("Adder", adder conn), ("Xml", xmlTask), ("Find User", findUser conn)]
-- 
-- adder conn = startTask "adder" $ do 
--   display "Please register first."
--   user <- register
--   display $ show user
  -- userId <- gNew conn user
  -- display $ "Your user id is: " ++ show userId
  -- (x,(y, z)) <- input :: Task (Integer, (Integer, Integer))
  -- link "add the two numbers"
  -- display $ "The sum is : " ++ show (x + y)
  -- wrap (("Thanks, " +++ X.br) +++) $ display (view user)

