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
  to (User a b c d) = Field "name" (TString a) :*: Field "age" (TInteger b) :*: Field "email" (TString c) :*: Field "city" (TString d)
  from (Field _ (TString name) :*: Field _ (TInteger age) :*: Field _ (TString email) :*: Field _ (TString city)) = User name age email city
  rep _ = RField "name" RString `RProduct` (RField "age" RInteger `RProduct` (RField "email" RString `RProduct` RField "city" RString))
