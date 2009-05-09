> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE TypeOperators #-}
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

> data (:->) a b where
>   Box         :: (a -> r)        -> a :-> r
>   Action      :: (Show a, Read a) => (a -> Action b) -> a :-> b
>   Edge        :: (Show b, Read b) => a :-> b -> (b :-> c) -> (a :-> c)
>   Choice      :: (a -> Bool)     -> a :-> c -> a :-> c -> a :-> c

> type Trace = [Either () Bool]


Furthermore, an Action can be either a simple value (|Const|), a value that the user
has to provide (|Form|) or the display of a text-value.

> data Action a where
>   Const    :: a       ->                        Action a
>   IOAction :: IO a    ->                        Action a
>   Form     :: Form a  -> Maybe [String]      -> Action a
>   Wrapped  :: (X.Html -> X.Html) -> Action a -> Action a

> instance Functor Action where
>   fmap f (Const x)     = Const (f x)
>   fmap f (IOAction io) = IOAction (fmap f io)
>   fmap f (Form form m) = Form (fmap f form) m
>   fmap f (Wrapped h a) = Wrapped h (fmap f a)

We can store continuations like this:

> data Cont b where
>   Cont :: (Show x, Read x) => x -> x :-> b -> Cont b

The FromAction class is a utility class that allows us for nice polymorphic programming. 
We can modify (wrap) our actions and still have a Task as result. The compiler will 
inference this for us.

> class FromAction f where
>   fromAction :: Action a -> f a
>
> instance FromAction Action where
>   fromAction = id
>
> instance (Read a, Show a) => FromAction ((:->) a) where
>   fromAction = Action . const

Now, because we have the FromAction class, we can define a generic wrap function that can modify the html.

> wrap :: FromAction f => (X.Html -> X.Html) -> Action a -> f a
> wrap x = fromAction . Wrapped x
>

Finally, we need some convenience types.

> type URL = String
> type Env = [(URL, (Trace, FormData -> Cont ()))]
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

> form f = fromAction $ Form f Nothing
>
> instance Applicative Identity where
>   pure  = return
>   (<*>) = ap
>

Some default instances for convenience

> instance (Show a) => Show (Action a) where
>   show (Const x)     = "Const: " ++ (show x)
>   show (Wrapped _ _) = "Wrapped"
>   show (Form _ _ )   = "Form"
>
> instance Show (a :-> b) where
>   show (Box _ ) = "Box"
>   show (Edge l r) = "Edge (" ++ show l ++ ") (" ++ show r ++ ")"
>   show (Choice _ l r) = "Choice (" ++ show l ++ ") (" ++ show r ++ ")"
>   show (Action _) = "Interaction"
