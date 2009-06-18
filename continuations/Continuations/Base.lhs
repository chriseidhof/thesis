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
> import Data.Maybe (fromJust)

A |Form| consists of Html and a function that parses the user input. Due to the
nature of web applications, user input always arrives as a list of key/value
pairs, where both the keys and the values are strings. 

Now, if we have an action, we want to be able to evaluate its result given the
FormData:

> eval :: Action a -> FormData -> IO (Failing a)
> eval (Const x)     e = return $ Success x
> eval (Wrapped f x) e = eval x e
> eval (IOAction i)  e = Success <$> i
> eval (PendingForm f _ ) e = return $ runIdentity compValue
>   where (compValue, _, _) = runFormState e "" f
> eval x e = error $ "No eval defined for: " ++ showType x

> unwrap (Wrapped _ x) = unwrap x
> unwrap x             = x


Now, if we get a request for a page, we have to look it up in the environment.
The page will display some Html and possibly extend the environment with a new
continuation.

> run :: Env -> URL -> FormData -> IO (Html, Env)
> run env page post = case lookup page env of
>   Nothing -> return (error $ "Task not found: " ++ page ++ show (map fst env), env)
>   Just (tr, cont) -> do let freshUrl = (show $ length env)
>                         (html, next) <- run' freshUrl post tr cont
>                         let env' = maybe env (\n -> (freshUrl, n):env) next
>                         return (html, env')

> run' :: URL -> FormData -> Trace -> Cont () -> IO (Html, Maybe (Trace, Cont ()))
> run' freshUrl post tr (Cont x c) = do
>   (tr', (Cont x' c')) <- takeSteps post tr (Cont x $ restructure c)
>   case restructure c' of
>              (Action i)          -> return (maybe noHtml id $ mkHtml post freshUrl False (i x'), Nothing)
>              (Edge (Action i) r) -> do let html    = mkHtml post freshUrl True (i x')
>                                        case (unwrap $ i x') of           -- TODO: we should trace something below
>                                          Form f msgs        -> return (fromJust $ html, Just (tr', Cont x' (Edge (Action (const $ PendingForm f msgs)) r)))
>                                          action -> do 
>                                             result <- eval action post
>                                             case result of
>                                               Failure msgs -> return (fromJust html, Just (tr', Cont x' $ Edge (Action i) r))
>                                               Success x    -> case html of
>                                                                 Nothing -> run' freshUrl post (tr' ++ [TrEdge]) (Cont x r)
>                                                                 Just h -> return (h, Just (tr' ++ [TrEdge], (Cont x r)))
>                                     
>              x  -> error $ "Run not defined for " ++ show x
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

> restructure :: a :-> b -> a :-> b
> restructure (Edge l r) = case restructure l of
>                            (Edge l'' r') -> Edge l'' (restructure (Edge r' r))
>                            _ -> Edge l r
> restructure x = x

> takeSteps :: FormData -> Trace -> Cont b -> IO (Trace, Cont b)
> takeSteps postData tr cont@(Cont x (Edge (Action a)     r)) = case a x of
>   action@(PendingForm f _) -> do result <- eval action postData
>                                  case result of
>                                    Failure msgs -> return (tr, Cont x (Edge (Action $ const $ PendingForm f (Just msgs)) r))
>                                    Success x -> do let tr' = tr ++ [TrEdge]
>                                                    takeSteps postData tr' (Cont x r)
>                             
>   (Const c) -> do let tr' = tr ++ [TrEdge]
>                   takeSteps postData tr' (Cont c r)
>   (IOAction io) -> do c <- io
>                       let tr' = tr ++ [TrEdge]
>                       takeSteps postData tr' (Cont c r)
>
>   _ -> return (tr, cont)
> takeSteps postData tr cont@(Cont x (Edge (Choice cond l' r')    r)) = do let condVal = cond x
>                                                                              tr' = tr ++ [TrChoice condVal]
>                                                                          takeSteps postData tr' (Cont x $ restructure $ (if condVal then l' else r') `Edge` r)
> --TODO: almost exactly like above
> takeSteps postData tr cont@(Cont x (Choice cond l' r')) = do let condVal = cond x
>                                                                  tr' = tr ++ [TrChoice condVal]
>                                                              takeSteps postData tr' (Cont x $ restructure $ (if condVal then l' else r'))
> takeSteps postData tr x = return (tr, x)

Now, some handy utility functions

> mkHtml :: FormData -> URL -> Bool -> Action a -> Maybe X.Html
> -- TODO: duplication (they're exactly the same)
> mkHtml d url continue (PendingForm f msgs) = Just $ X.form ! [X.action $ "/" ++ url, X.method "POST"] << (html +++ X.br +++ X.submit "next" "next")
>                                       where html = maybe noHtml unordList msgs +++ runIdentity formHtml
>                                             (_, formHtml, _) = runFormState d "" f
> mkHtml d url continue (Form f msgs) = Just $ X.form ! [X.action $ "/" ++ url, X.method "POST"] << (html +++ X.br +++ X.submit "next" "next")
>                                       where html = maybe noHtml unordList msgs +++ runIdentity formHtml
>                                             (_, formHtml, _) = runFormState d "" f
> mkHtml d url False    (Wrapped f w) = Just $ (f $ maybe noHtml id $ mkHtml d url False w)
> mkHtml d url True     (Wrapped f w) = Just $ (f $ maybe noHtml id $ mkHtml d url False w) +++ X.br +++ X.anchor ! [X.href $ "/" ++ url] << ("next")
> mkHtml d url continue _             = Nothing

> addMessages :: Action a -> [String] -> Action a
> addMessages (Wrapped f w) ms = Wrapped f (addMessages w ms)
> addMessages (Form f _) ms    = (Form f $ Just ms)
> addMessages _          _     = error "Internal error: addMessages not possible for this action"
