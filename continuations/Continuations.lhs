> module Continuations ( module Continuations.Base
>                      , module Continuations.Generics  
>                      , module Continuations.Actions
>                      , input) 
> where

> import Continuations.Base
> import Continuations.Generics
> import Continuations.Actions
> import Continuations.Types

> input :: (SimpleInput a, FromAction task) => task a
> input = form simpleInput
