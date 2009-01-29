> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> module Continuations where

> import qualified Text.XHtml.Strict as X
> import Text.XHtml.Strict hiding (URL, input, form)
> import Text.XHtml.Strict.Formlets (XHtmlForm, runFormState)
> import Control.Applicative.Error (Failing (..))
> import qualified Text.XHtml.Strict.Formlets as F
> import Control.Monad.Identity (Identity, runIdentity)
> import Continuations.Types
> import Data.List (intersperse)

> instance (Show a) => Show (Action a) where
>   show (Display h) = "Display: " ++ show h
>   show (Link text) = "Link: " ++ show text
>   show (Const x)   = "Const: " ++ (show x)
>   show (Form _ _ ) = "Form"

> instance HTML (Action a) where
>   toHtml (Display d)   = d
>   toHtml (Link h)      = toHtml "link"
>   toHtml (Const x)     = toHtml "const"
>   toHtml (Form  f _)   = runIdentity html
>     where (_, html, _) = runFormState [] "" f
> 
> instance (Show a) => Show (Task a) where
>   show (Single x) = "Endpoint: " ++ show x
>   show (Step x y)   = "Partial application, left of the bind is a " ++ (case x of {Single _ -> "single"; _ -> "step"})

A |Form| consists of Html and a function that parses the user input. Due to the
nature of web applications, user input always arrives as a list of key/value
pairs, where both the keys and the values are strings. 

Readers familiar with Monads will have spotted a strong similarity between the
datatype definition of |Task| and Monad operations. Indeed, we can easily make
our Tasks an instance of |Monad|:

> instance Monad Task where
>   return = Single . Const
>   (>>=)  = Step

Now, if we have an action, we want to be able to evaluate its result given the
FormData:

> eval :: Action a -> FormData -> Failing a
> eval (Const x)     e = Success x
> eval (Link s)      e = Success ()
> eval (Display s)   e = Success ()
> eval (Form f _ )   e = runIdentity compValue
>   where (compValue, _, _) = runFormState e "" f


Now, if we get a request for a page, we have to look it up in the environment.
The page will display some Html and possibly extend the environment with a new
continuation.

> mkForm :: FormData -> Action a -> URL -> X.Html
> mkForm d (Form f msgs) url = X.form ! [X.action $ "/" ++ url, X.method "POST"] << (html +++ X.submit "next" "next")
>                              where html = maybe noHtml unordList msgs +++ runIdentity formHtml
>                                    (_, formHtml, _) = runFormState d "" f
> mkForm d (Link txt)    url = X.anchor ! [X.href $ "/" ++ url] << txt
> mkForm d f             url = f +++ X.br +++ X.anchor ! [X.href $ "/" ++ url] << ("next")

> choices = X.concatHtml . intersperse X.br . map (\(n, StartTask url _) ->
>             X.anchor ! [X.href $ "/" ++ url] << n)

> run :: Env -> URL -> FormData -> (Html, Env)
> run env page post = case lookup page env of
>   Nothing -> (error "Task not found", env)
>   Just  x -> case restructure (x post) of
>                   (Single a)  -> (toHtml a, env)
>                   (Choice cs) -> (choices cs, env)
>                   (Step (Single f) cont) -> (mkForm post f nextUrl, ((nextUrl, newEnv):env))
>                    where nextUrl = show (length env)
>                          newEnv :: FormData -> Task ()
>                          newEnv p = case eval f p of
>                            Success v    -> cont v
>                            Failure msgs -> (Step (Single (addMessages f msgs)) cont)
>
> addMessages :: Action a -> [String] -> Action a
> addMessages (Form f _) ms = (Form f $ Just ms)
> addMessages _          _  = error "Internal error: addMessages not possible for this action"

The careful reader will see a possible problem with the code above: the patterns
look like they are non-exhaustive. However, the function |restructure|
restructures the spine of a task so that it will either be a |Single| or a
|Step| with only a |Single| action on the left. 

We can use the Associativity monad law to change the spine: |(m >>= f) >>= g = m >>= (\x -> f x >>= g)|

> restructure :: Task a -> Task a
> restructure x@(Single _)    = x
> restructure x@(Choice _)    = x
> restructure (Step arg cont) = case restructure arg of
>                                    (Single _)      -> Step arg cont
>                                    (Choice _)      -> Step arg cont
>                                    Step arg' cont' -> Step arg' (\x -> cont' x >>= cont)

Now, some handy utility functions.

> form f = Single $ Form f Nothing
> 
> link :: HTML a => a -> Task ()
> link =  Single . Link . toHtml

> display :: HTML a => a -> Task ()
> display =  Single . Display . toHtml

> choice :: [(String, StartTask)] -> Task ()
> choice = Choice

> startTask = StartTask

And some default inputs

> class DefaultTask a where
>   input :: Task a
> 
> instance SimpleInput a => DefaultTask a where
>   input = form simpleInput
