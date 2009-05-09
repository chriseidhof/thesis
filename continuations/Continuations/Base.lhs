> {-# LANGUAGE TypeOperators #-}
> module Continuations.Base (run) where

> import qualified Text.XHtml.Strict as X
> import Text.XHtml.Strict hiding (URL, input, form)
> import Text.XHtml.Strict.Formlets (XHtmlForm, runFormState)
> import Control.Applicative.Error (Failing (..))
> import qualified Text.XHtml.Strict.Formlets as F
> import Control.Monad.Identity (Identity, runIdentity)
> import Continuations.Types
> import Control.Applicative hiding (Const)

A |Form| consists of Html and a function that parses the user input. Due to the
nature of web applications, user input always arrives as a list of key/value
pairs, where both the keys and the values are strings. 

Now, if we have an action, we want to be able to evaluate its result given the
FormData:

> eval :: Action a -> FormData -> IO (Failing a)
> eval (Const x)     e = return $ Success x
> eval (Wrapped f x) e = eval x e
> eval (IOAction i)  e = Success <$> i
> eval (Form f _ )   e = return $ runIdentity compValue
>   where (compValue, _, _) = runFormState e "" f


Now, if we get a request for a page, we have to look it up in the environment.
The page will display some Html and possibly extend the environment with a new
continuation.

> run :: Env -> URL -> FormData -> IO (Html, Maybe (Trace, Cont ()))
> run env page post = case lookup page env of
>   Nothing -> return (error $ "Task not found: " ++ page ++ show (map fst env), Nothing)
>   Just (tr, cont) -> do (Cont x c) <- return $ cont post
>                         (tr', (Cont x' c')) <- takeSteps tr (Cont x $ restructure c)
>                         case restructure c' of
>                                    (Box f)             -> return (noHtml, Nothing)
>                                    (Action i)          -> return (mkHtml post page False (i x'), Nothing)
>                                    (Edge (Action i) r) -> undefined
>                                    (Choice cond l r)   -> undefined --t
> 
>                 --case res of
>                 --  (Single a)    -> return (mkHtml post page False a, env)
>                 --  (Step (Single f) cont) -> return (mkHtml post nextUrl True f, ((nextUrl, newEnv):env))
>                 --   where nextUrl = show (length env)
>                 --         newEnv :: FormData -> IO (Task ())
>                 --         newEnv p = do x <- eval f p
>                 --                       return $ case x of
>                 --                         Success v    -> cont v
>                 --                         Failure msgs -> Step (Single (addMessages f msgs)) cont
>                 --  _ -> return (error "Restructuring error", env)

We can use the Arrow laws to change the spine. After executing the arrow law, the left part of the topmost Edge will not contain an Edge.

> -- TODO: maybe account for 
> restructure :: a :-> b -> a :-> b
> restructure (Edge l r) = case restructure l of
>                            (Edge l'' r') -> Edge l'' (restructure (Edge r' r))
>                            _ -> Edge l r
> restructure x = x

> takeSteps :: Trace -> Cont b -> IO (Trace, Cont b)
> takeSteps tr cont@(Cont x (Edge (Action a)     r)) = case a x of
>                                                   (Const c) -> do let tr' = tr ++ [Left ()]
>                                                                   takeSteps tr' (Cont c r)
>                                                   (IOAction io) -> do c <- io
>                                                                       let tr' = tr ++ [Left ()]
>                                                                       takeSteps tr' (Cont c r)
>
>                                                   _ -> return (tr, cont)
> takeSteps tr cont@(Cont x (Edge (Choice cond l' r')    r)) = do let condVal = cond x
>                                                                     tr' = tr ++ [Right condVal]
>                                                                 takeSteps tr' (Cont x $ restructure $ (if condVal then l' else r') `Edge` r)
> takeSteps tr x = return (tr, x)

Now, some handy utility functions

> mkHtml :: FormData -> URL -> Bool -> Action a -> X.Html
> mkHtml d url continue (Form f msgs) = X.form ! [X.action $ "/" ++ url, X.method "POST"] << (html +++ X.br +++ X.submit "next" "next")
>                                       where html = maybe noHtml unordList msgs +++ runIdentity formHtml
>                                             (_, formHtml, _) = runFormState d "" f
> mkHtml d url continue (Wrapped f w) = f $ mkHtml d url continue w
> mkHtml d url continue (Const x)     = X.toHtml "const"
> mkHtml d url continue (IOAction _)  = X.toHtml "ioaction"

> addMessages :: Action a -> [String] -> Action a
> addMessages (Wrapped f w) ms = Wrapped f (addMessages w ms)
> addMessages (Form f _) ms    = (Form f $ Just ms)
> addMessages _          _     = error "Internal error: addMessages not possible for this action"
