{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Model where

import Generics.Records
import Generics.SimpleInput
import Continuations.Types
import Control.Applicative

data User = User {name :: String, age :: Integer, email :: String, city :: String}

-- TODO: this should be template haskell.
instance Representable User (String, (Integer, (String, String))) where
  to (User a b c d)     = a & b & c & d
  from (a, (b, (c, d))) = User a b c d
  rep _ =  Field "name"  RString
       :*: Field "age"   RInteger 
       :*: Field "email" RString 
       :*: Field "city"  RString
