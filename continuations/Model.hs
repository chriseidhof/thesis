{-# LANGUAGE TypeFamilies #-}
module Model where

import Generics.Records
import Generics.SimpleInput
import Continuations.Types
import Control.Applicative
import Generics.Views

data User = User {name :: String, age :: Integer, email :: String, city :: String}
data Post = Post {title :: String, body :: String, date :: String}
type instance PF User = (String, (Integer, (String, String)))
type instance PF Post = (String, (String, String))

-- TODO: this should be template haskell.
instance Representable User  where
  undef = undefined
  to (User a b c d)     = a & b & c & d
  from (a, (b, (c, d))) = User a b c d
  rep _ =  Con "User" $ Field "name"  RString
       :*: Field "age"   RInteger 
       :*: Field "email" RString 
       :*: Field "city"  RString
