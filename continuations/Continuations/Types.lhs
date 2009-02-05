> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> module Continuations.Types where
> 
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as F
> import Control.Monad.Identity (Identity, runIdentity)
> import Control.Monad (ap)
> import Control.Applicative hiding (Const)
> 
> type Form a = F.XHtmlForm Identity a
> type FormData = F.Env

We start with a representation of a |Task|. A |Task| can be either a single
|Action| (for example, displaying a value) or a series of steps. Every |Task| is
indexed by its result. The result from the first step can be used in the next
steps:

> type StartTasks = [(String, StartTask)]

> data Task a where
>   Single  :: Action a -> Task a
>   Choice  :: (StartTasks -> X.Html) -> StartTasks -> Task ()
>   Step    :: Task a -> (a -> Task b) -> Task b
>   Start   :: String -> Task ()

> data StartTask = StartTask String (Task ())

Furthermore, an Action can be either a simple value (|Const|), a value that the user
has to provide (|Form|) or the display of a text-value.

> data Action a where
>   Const    :: a -> Action a
>   IOAction :: IO a -> Action a
>   Form     :: Form a -> Maybe [String] -> Action a
>   Display  :: X.Html -> Action ()
>   Link     :: X.Html -> Action ()
>   Wrapped  :: (X.Html -> X.Html) -> Action a -> Action a

The FromAction class is a utility class that allows us for nice polymorphic programming. 
We can modify (wrap) our actions and still have a Task as result. The compiler will 
inference this for us.

> class FromAction f where
>   fromAction :: Action a -> f a
>
> instance FromAction Action where
>   fromAction = id
>
> instance FromAction Task where
>   fromAction = Single

Now, because we have the FromAction class, we can define a generic wrap function that can modify the html.

> wrap :: FromAction f => (X.Html -> X.Html) -> Action a -> f a
> wrap x = fromAction . Wrapped x
>

Finally, we need some convenience types.

> type URL = String
> type Env = [(URL, FormData -> IO (Task ()))]
> emptyEnv = [] :: Env

This provides us with default inputs for certain datatypes.

> class SimpleInput a where
>   simpleInput :: Form a
> 
> instance SimpleInput Integer where
>   simpleInput = F.inputInteger Nothing
> 
> instance SimpleInput String where
>   simpleInput = F.input Nothing
> 
> instance (SimpleInput a, SimpleInput b) => SimpleInput (a, b) where
>  simpleInput = (,) <$> simpleInput <*> simpleInput
>
> instance (SimpleInput a, SimpleInput b, SimpleInput c) => SimpleInput (a, b, c) where
>  simpleInput = (,,) <$> simpleInput <*> simpleInput <*> simpleInput

Once we can generate forms for our datatypes, we can also generate tasks. But because we want to use the |wrap| 
function, we generate either an |Action| or a |Task|.

> class DefaultTask a where
>   input :: (FromAction f) => f a
> 
> instance SimpleInput a => DefaultTask a where
>   input = form simpleInput
>
> form f = fromAction $ Form f Nothing
>
> instance Applicative Identity where
>   pure  = return
>   (<*>) = ap
>

Some default instances for convenience

> instance (Show a) => Show (Action a) where
>   show (Display h)   = "Display: " ++ show h
>   show (Link text)   = "Link: " ++ show text
>   show (Const x)     = "Const: " ++ (show x)
>   show (Wrapped _ _) = "Wrapped"
>   show (Form _ _ )   = "Form"
>
> instance (Show a) => Show (Task a) where
>   show (Single x) = "Endpoint: " ++ show x
>   show (Step x y) = "Partial application, left of the bind is a " ++ (case x of {Single _ -> "single"; _ -> "step"})
