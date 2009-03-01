{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Generics.Records where

data Rep r where
  RInt     :: Rep Int
  RInteger :: Rep Integer
  RString  :: Rep String
  Field    :: String -> Rep r -> Rep r
  (:*:)    :: Rep r1 -> Rep r2 -> Rep (r1,r2)
--  Con      :: String -> Rep r -> Rep r

infixr :*:

(&) = (,)
infixr &

type family PF a

class Representable a where
  to    :: a -> PF a
  from  :: PF a -> a
  rep   :: a -> Rep (PF a)
  undef :: a
