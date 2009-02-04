{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Records where

data Rep a r where
  TInt     :: Int -> Rep a Int
  TInteger :: Integer -> Rep a Integer
  TString  :: String -> Rep a String
  Field    :: String -> Rep a r -> Rep a r
  (:*:)    :: Rep a r1 -> Rep a r2 -> Rep a (r1, r2)

instance Show (Rep a r) where
  show (TInt x) = show x
  show (TInteger x) = show x
  show (TString s) = show s
  show (Field lbl val) = lbl ++ " = " ++ show val
  show (a :*: b) = show a ++ ", " ++ show b

data RepT a r where
  RInt     :: RepT a Int
  RInteger :: RepT a Integer
  RString  :: RepT a String
  RField   :: String -> RepT a r -> RepT a r
  RProduct :: RepT a r1 -> RepT a r2 -> RepT a (r1,r2)


infixr :*:  

class Representable a r | a -> r where
  to   :: a -> Rep a r
  from :: Rep a r -> a
  rep  :: a -> RepT a r

instance Representable String String where
  to = TString
  from (TString s) = s
  rep _ = RString
