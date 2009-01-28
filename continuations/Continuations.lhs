> {-# LANGUAGE GADTs #-}
> module Continuations where
> import Text.XHtml.Strict hiding (URL)
> import qualified Text.XHtml.Strict as X
> import Text.XHtml.Strict.Formlets (XHtmlForm, runFormState)
> import Control.Applicative.Error (Failing (..))
> import qualified Text.XHtml.Strict.Formlets as F
> import Control.Monad.Identity (Identity, runIdentity)

We start with a representation of a |Task|. A |Task| can be either a single
|Action| (for example, displaying a value) or a series of steps. Every |Task| is
indexed by its result. The result from the first step can be used in the next
steps:

> data Task a where
>   Single :: Action a -> Task a
>   Step   :: Task a -> (a -> Task b) -> Task b

Furthermore, an Action can be either a simple value (|Const|), a value that the user
has to provide (|Form|) or the display of a text-value.

> data Action a where
>   Const   :: a -> Action a
>   Form    :: Form a -> Action a
>   Display :: Html -> Action ()

TODO: hide these instances in the resulting document

> instance (Show a) => Show (Action a) where
>   show (Display text) = "Display: " ++ show text
>   show (Const x)     = "Const: " ++ (show x)
>   show (Form x)      = "Form"

> instance HTML (Action a) where
>   toHtml (Display h)   = h
>   toHtml (Const x)     = toHtml "const"
>   toHtml (Form  f)     = runIdentity html
>     where (_, html, _) = runFormState [] "" f
> 
> instance (Show a) => Show (Task a) where
>   show (Single x) = "Endpoint: " ++ show x
>   show (Step x y)   = "Partial application, left of the bind is a " ++ (case x of {Single _ -> "single"; _ -> "step"})

A |Form| consists of Html and a function that parses the user input. Due to the
nature of web applications, user input always arrives as a list of key/value
pairs, where both the keys and the values are strings. 

> type Form a = XHtmlForm Identity a
> type FormData = F.Env

Readers familiar with Monads will have spotted a strong similarity between the
datatype definition of |Task| and Monad operations. Indeed, we can easily make
our Tasks an instance of |Monad|:

> instance Monad Task where
>   return = Single . Const
>   (>>=)  = Step

Now, if we have an action, we want to be able to evaluate its result given the
FormData:

> eval :: Action a -> FormData -> a
> eval (Const x)     e = x
> eval (Display s)   e = ()
> eval (Form f) e      = case runIdentity compValue of
>                          Success x    -> x
>                          Failure msgs -> error (concat msgs)
>   where (compValue, _, _) = runFormState e "" f

To run a real website, we need to keep an environment mapping URLs to Tasks.

> type URL = String
> type Env = [(URL, FormData -> Task ())]
> emptyEnv = [] :: Env

Now, if we get a request for a page, we have to look it up in the environment.
The page will display some Html and possibly extend the environment with a new
continuation.

> mkForm :: Action a -> URL -> X.Html
> mkForm f url = X.form ! [X.action $ "/" ++ url, X.method "POST"] << (f +++ X.submit "submit" "submit")

> run :: Env -> URL -> FormData -> (Html, Env)
> run env page post = case lookup page env of
>   Nothing -> error "Task not found"
>   Just  x -> case restructure (x post) of
>                   (Single a)  -> (toHtml a, env)
>                   (Step (Single f) cont) -> (mkForm f nextUrl, (nextUrl, \p -> cont (eval f p)):env)
>                    where nextUrl = show (length env)

The careful reader will see a possible problem with the code above: the patterns
look like they are non-exhaustive. However, the function |restructure|
restructures the spine of a task so that it will either be a |Single| or a
|Step| with only a |Single| action on the left. 

We can use the Associativity monad law to change the spine: |(m >>= f) >>= g = m >>= (\x -> f x >>= g)|

> restructure :: Task a -> Task a
> restructure x@(Single _)    = x
> restructure (Step arg cont) = case restructure arg of
>                                    (Single _)      -> Step arg cont
>                                    Step arg' cont' -> Step arg' (\x -> cont' x >>= cont)

Now, some handy utility functions.

> form = Single . Form
> 
> display :: HTML a => a -> Task ()
> display =  Single . Display . toHtml
