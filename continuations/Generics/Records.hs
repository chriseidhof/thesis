{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Records where

data Rep r where
  RInt     :: Rep Int
  RInteger :: Rep Integer
  RString  :: Rep String
  Field    :: String -> Rep r -> Rep r
  (:*:)    :: Rep r1 -> Rep r2 -> Rep (r1,r2)

infixr :*:

(&) = (,)
infixr &

class Representable a r | a -> r where
  to   :: a -> r
  from :: r -> a
  rep  :: a -> Rep r

instance Representable String String where
  to = id
  from = id
  rep _ = RString
