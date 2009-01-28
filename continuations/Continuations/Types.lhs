> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> module Continuations.Types where
> 
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as F
> import Control.Monad.Identity (Identity, runIdentity)
> import Control.Monad (ap)
> import Control.Applicative
> 
> type Form a = F.XHtmlForm Identity a
> type FormData = F.Env

We start with a representation of a |Task|. A |Task| can be either a single
|Action| (for example, displaying a value) or a series of steps. Every |Task| is
indexed by its result. The result from the first step can be used in the next
steps:

> data Task a where
>   Single :: Action a -> Task a
>   Choice :: [(String, StartTask)] -> Task ()
>   Step   :: Task a -> (a -> Task b) -> Task b

> data StartTask = StartTask {url :: String, task :: Task ()}

Furthermore, an Action can be either a simple value (|Const|), a value that the user
has to provide (|Form|) or the display of a text-value.

> data Action a where
>   Const   :: a -> Action a
>   Form    :: Form a -> Action a
>   Display :: X.Html -> Action ()
>   Link    :: X.Html -> Action ()

> type URL = String
> type Env = [(URL, FormData -> Task ())]
> emptyEnv = [] :: Env
> 
> class SimpleInput a where
>   simpleInput :: Form a
> 
> instance SimpleInput Integer where
>   simpleInput = F.inputInteger Nothing
> 
> instance SimpleInput String where
>   simpleInput = F.input Nothing
>
> instance Applicative Identity where
>   pure  = return
>   (<*>) = ap
> 
> instance (SimpleInput a, SimpleInput b) => SimpleInput (a, b) where
>  simpleInput = (,) <$> simpleInput <*> simpleInput