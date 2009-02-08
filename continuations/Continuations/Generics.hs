module Continuations.Generics where

import Continuations.Types
import Generics.Records
import Generics.SimpleInput
import Generics.Documentation
import Generics.HDBC
import Database.HDBC
import Continuations.Base
import Continuations.Actions

-- Generic views

gInput :: (Representable a, FromAction f) => f a
gInput = form (gRepInput Nothing)

gEdit :: (Representable a, FromAction f) => a -> f a
gEdit = form . gRepInput . Just

-- Generate documentation for a datatype

gDoc :: (Representable a, FromAction f) => a -> f ()
gDoc = display . documentation

-- Generic database actions.

gNew :: (Representable a, FromAction f, IConnection i) => i -> a -> f Integer
gNew c x = ioAction $ new c x

gFind :: (Representable a, FromAction f, IConnection i) => i -> Integer -> f (Maybe a)
gFind c x = ioAction $ find undef c x

gFindAll :: (Representable a, FromAction f, IConnection i) => i -> f [(Integer, a)]
gFindAll c = ioAction $ findAll undef c
