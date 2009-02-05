> module Continuations where

> import qualified Text.XHtml.Strict as X
> import Text.XHtml.Strict hiding (URL, input, form)
> import Text.XHtml.Strict.Formlets (XHtmlForm, runFormState)
> import Control.Applicative.Error (Failing (..))
> import qualified Text.XHtml.Strict.Formlets as F
> import Control.Monad.Identity (Identity, runIdentity)
> import Continuations.Types
> import Data.List (intersperse)
> import Generics.Records
> import Generics.SimpleInput
> import Generics.Documentation
> import Generics.HDBC
> import Control.Applicative hiding (Const)
> import Database.HDBC

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

> eval :: Action a -> FormData -> IO (Failing a)
> eval (Const x)     e = return $ Success x
> eval (Link s)      e = return $ Success ()
> eval (Display s)   e = return $ Success ()
> eval (Wrapped f x) e = eval x e
> eval (IOAction i)  e = Success <$> i
> eval (Form f _ )   e = return $ runIdentity compValue
>   where (compValue, _, _) = runFormState e "" f


Now, if we get a request for a page, we have to look it up in the environment.
The page will display some Html and possibly extend the environment with a new
continuation.

> mkHtml :: FormData -> URL -> Bool -> Action a -> X.Html
> mkHtml d url continue (Form f msgs) = X.form ! [X.action $ "/" ++ url, X.method "POST"] << (html +++ X.br +++ X.submit "next" "next")
>                                       where html = maybe noHtml unordList msgs +++ runIdentity formHtml
>                                             (_, formHtml, _) = runFormState d "" f
> mkHtml d url continue (Link txt)    = X.anchor ! [X.href $ "/" ++ url] << txt
> mkHtml d url continue (Wrapped f w) = f $ mkHtml d url continue w
> mkHtml d url True     (Display h)   = h +++ X.br +++ X.anchor ! [X.href $ "/" ++ url] << ("next")
> mkHtml d url False    (Display h)   = h
> mkHtml d url continue (Const x)     = X.toHtml "const"
> mkHtml d url continue (IOAction _)  = X.toHtml "ioaction"


> run :: Env -> URL -> FormData -> IO (Html, Env)
> run env page post = case lookup page env of
>   Nothing -> return (error $ "Task not found: " ++ page ++ show (map fst env), env)
>   Just  x -> do res <- x post >>= restructure
>                 case res of
>                   (Single a)    -> return (mkHtml post page False a, env)
>                   (Choice f cs) -> return (f cs, env)
>                   (Step (Single f) cont) -> return (mkHtml post nextUrl True f, ((nextUrl, newEnv):env))
>                    where nextUrl = show (length env)
>                          newEnv :: FormData -> IO (Task ())
>                          newEnv p = do x <- eval f p
>                                        return $ case x of
>                                          Success v    -> cont v
>                                          Failure msgs -> Step (Single (addMessages f msgs)) cont
>                   _ -> return (error "Restructuring error", env)
>

The careful reader will see a possible problem with the code above: the patterns
look like they are non-exhaustive. However, the function |restructure|
restructures the spine of a task so that it will either be a |Single| or a
|Step| with only a |Single| action on the left. 

We can use the Associativity monad law to change the spine: |(m >>= f) >>= g = m >>= (\x -> f x >>= g)|

> restructure :: Task a -> IO (Task a)
> restructure x@(Single _)    = return x
> restructure x@(Choice _ _)  = return x
> restructure (Step arg cont) = do r <- restructure arg
>                                  case r of
>                                      (Single (IOAction a)) -> a >>= restructure . cont
>                                      (Single (Const x))    -> restructure $ cont x
>                                      (Single _)            -> return $ Step arg cont
>                                      (Choice _ _)          -> return $ Step arg cont
>                                      Step arg' cont'       -> return $ Step arg' (\x -> cont' x >>= cont)

Now, some handy utility functions.

> link :: (HTML a, FromAction f) => a -> f ()
> link =  fromAction . Link . toHtml

> display :: (HTML a, FromAction f) => a -> f ()
> display =  fromAction . Display . toHtml
>
> ioAction :: (FromAction f) => IO a -> f a
> ioAction = fromAction . IOAction
>
> choice :: [(String, StartTask)] -> Task ()
> choice = Choice choices
> 

> choices = X.concatHtml . intersperse X.br . map (\(n, StartTask url _) ->
>             X.anchor ! [X.href $ "/" ++ url] << n)
>
> startTask = StartTask

> label :: String -> Form a -> Form a
> label text = F.plug (\f -> (X.label << text) +++ f)

> addMessages :: Action a -> [String] -> Action a
> addMessages (Wrapped f w) ms = Wrapped f (addMessages w ms)
> addMessages (Form f _) ms    = (Form f $ Just ms)
> addMessages _          _     = error "Internal error: addMessages not possible for this action"

Generalized inputs:

> gInput :: (Representable a r, FromAction f) => f a
> gInput = form (gRepInput Nothing)

> gEdit :: (Representable a r, FromAction f) => a -> f a
> gEdit = form . gRepInput . Just

> gDoc :: (Representable a r, FromAction f) => a -> f ()
> gDoc = display . documentation

> gNew :: (Representable a r, FromAction f, IConnection i) => i -> a -> f Integer
> gNew c x = ioAction $ new c x

> gFind :: (Representable a r, FromAction f, IConnection i) => i -> Integer -> f (Maybe a)
> gFind c x = ioAction $ find undef c x

> gFindAll :: (Representable a r, FromAction f, IConnection i) => i -> f [(Integer, a)]
> gFindAll c = ioAction $ findAll undef c
