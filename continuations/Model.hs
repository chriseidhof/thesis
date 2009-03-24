{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Model where

import Generics.EMGM hiding (Show, show, Read, read)
import Generics.Records
import qualified Generics.Records as R
import Generics.SimpleInput
import Continuations.Types
import Control.Applicative
import Generics.Views

data User = User {name :: String, age :: Integer, email :: String, city :: String}
 deriving (Show, Read, Eq)
$(derive ''User)
data Post = Post {title :: String, body :: String, date :: String}
 deriving (Show, Read, Eq)
type instance PF User = (String, (Integer, (String, String)))
type instance PF Post = (String, (String, String))


-- TODO: this should be template haskell.
instance Representable User  where
  undef = undefined
  to (User a b c d)     = a & b & c & d
  from (a, (b, (c, d))) = User a b c d
  rep _ =  {- Con "User" $ -} Field "name"  RString
       R.:*: Field "age"   RInteger 
       R.:*: Field "email" RString 
       R.:*: Field "city"  RString
