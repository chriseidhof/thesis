{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sample where

import SampleModel
import Control.Applicative
import Continuations
import Continuations.Salvia
import Continuations.Types
import Database.HDBC (commit)
import Database.HDBC.Sqlite3
import Generics.Records (Rep, rep)
import Generics.Records.ModelName (ModelName)
import Generics.Records.Relations
import Text.XHtml.Strict ((+++))
import Text.XHtml.Strict.Formlets (enumSelect)
import qualified Generics.Records.Database as DB
import qualified Generics.Records.Forms    as GF
import qualified Text.XHtml.Strict as X

main :: IO ()
main = runServer 8016 [sample]

data CRU = Create | List
 deriving (Show, Read, Eq, Enum, Bounded)

sample = form (enumSelect Nothing)
     >>> if_ (== Create) (create userType) list

create pt =  form (form' pt) 
         >>> dbNew 
         >>> display "Thanks"

list = display "List"

-- helpers
data PT a = Empty

userType = (Empty :: PT User)

form' :: Rep GF.Form a => PT a -> Form a
form' _ = GF.form Nothing

dbNew :: (Rep DB.Values a, Rep DB.Columns a, Rep ModelName a, Show a, Read a) => a :-> Int
dbNew = dbAction DB.new

dbAction :: (Show a, Read a) => (a -> DB.DB b) -> a :-> b
dbAction a = Action (IOAction . doIt)
 where doIt x = do conn <- connectSqlite3 "sample.sqlite3"
                   result <- DB.runDB conn (a x)
                   commit conn
                   return result

instance Rep GF.Form (BelongsTo Post) where
  rep = GF.ToForm (const $ pure BTNotFetched)

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

