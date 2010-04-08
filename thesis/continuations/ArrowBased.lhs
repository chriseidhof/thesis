%if False

> {-# LANGUAGE GADTs, ExistentialQuantification, TypeSynonymInstances,
> FlexibleContexts, PackageImports, Arrows #-}
> module Main where

> import Control.Applicative
> import Control.Applicative.Error (Failing (..))
> import "mtl" Control.Monad.Identity (Identity (..))
> import "transformers" Control.Monad.Trans
> import Control.Monad.Reader hiding (liftIO)
> import qualified Data.ByteString.Lazy.Char8 as B
> import Control.Arrow
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as Formlets
> import qualified Data.Map as M
> import Control.Concurrent.MVar
> import Data.List (intercalate)
> import Data.Maybe (isJust, fromJust)

> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Protocol.Uri.Data
> import Network.Salvia.Handlers
> import Network.Salvia.Impl.Config
> import Network.Salvia.Impl.Server
> import Network.Salvia.Interface
> import Data.Record.Label
> import Control.Category
> import Prelude as Prelude hiding ((.))
> import Network.Protocol.Http.Data


> type RequestBody = [(String,String)]

%endif

As we have seen in the previous section, web continuations can be expressed as
monads.
However, because the monadic approach stores functions inside the |Web| and
|Result| datatypes, it is not possible to serialize these datatypes.

Arrows are a different way of representing control structures, and an arrow
differs from a monad because the environment is always explicit.
For example, if we redefine the |Web| datatype for an arrow-based interface, we
add an extra type-parameter |i|, which captures the input of an arrow. The |o|
type-parameter indicates the result of running a |Web| computation.
Note that there are no functions stored in the |Web| type itself, which is
essential for serialization, as we will see later on.

> data Web i o where

The |Single| constructor indicates a single |Page| that is shown to the user.

>   Single  :: Page i o -> Web i o

The |Req| is a constructor that produces the |RequestBody| for a specific
request. It is needed to provide global access to the |RequestBody| throughout a
computation.

>   Req     :: Web () RequestBody

To compose to |Web| computations, we provide the |Seq| datatype.
 
>   Seq     :: Web a b -> Web b c -> Web a c

When we want to thread a value through a computation, we can use the |First|
constructor. It takes a computation, and adds an extra |c| value to the input and
the output.
When running the |Web| computation, the |c| value will be passed around
unchanged.

>   First   :: Web a b -> Web (a, c) (b, c)

Finally, we provide a constructor |Choice| for making choices. We will see how
to use this later on.

>   Choice  :: Web a b -> Web (Either a c) (Either b c)

We can make |Web| an instance for the |Functor|, |Arrow|, |ArrowChoice| and
|Category| type-classes, as seen in Figure \ref{fig:webinstances}.

\begin{figure}
> instance Functor (Web i) where
>   fmap = flip Seq . fun

> instance Arrow Web where
>   arr   = Single . Fun
>   first = First

> instance ArrowChoice Web where
>   left = Choice

> instance Category Web where
>   id  = fun Prelude.id
>   (.) = flip Seq
\caption{Instances for common type-classes}
\label{fig:webinstances}
\end{figure}

> instance Show (Web i o) where
>   show (Single s) = show s
>   show Req        = "Req"
>   show (Seq a b)  = show a ++ " >>> " ++ show b
>   show (First f)      = "First (" ++ show f ++ ")"
>   show (Choice a)    = "Choice (" ++ show a ++ ")"

\todo{Better explanation for |Thread|. Even a different type?}
\todo{Explain |Req|}

Our page datatype now has a similar structure, it is also parameterized over an
extra |i| for the input. To support arbitary functions, we have added a |Fun|
constructor.

> data Page i o where
>   Fun      :: (i -> o)                              -> Page i o
>   Form     :: X.Html -> (RequestBody -> Failing o)  -> Page a o
>   Display  :: (a -> X.Html)                         -> Page a ()
>   Link     :: String                                -> Page a ()

> instance Show (Page i o) where
>   show (Fun f) = "Fun"
>   show (Form f _) = "Form " ++ show f
>   show (Display f) = "Display"
>   show (Link l) = "link: " ++ l

We also provide smart constructors that lift a |Page| type into the |Web| type:

> fun = Single . Fun
> input :: DefaultForm f => Web () f
> input = Single (uncurry Form (runForm' form))
> display = Single . Display
> display' :: X.HTML a => a -> Web () ()
> display' a = Single (Display (const (X.toHtml a)))
> link = Single . Link

If we store the input of a |Web| value together with the |Web| value, we get a
continuation. We can hide the input type |i| using existential quantification.

> data Continuation o = forall i . Cont i (Web i o)

> instance Functor Continuation where
>   fmap f (Cont i w) = Cont i (fmap f w)
> 
> instance Show (Continuation o) where show (Cont i w) = take 120 $ show w

Now we can define the |Result| datatype, which is very similar to the |Result|
datatype from the previous section. However, our |Continuation| datatype does
not use functions to represent the environment.

> data Result o where
>   Done    :: o -> Result o
>   Step    :: X.Html -> Continuation o -> Result o

> instance Functor Result where
>   fmap f (Done x) = Done (f x)
>   fmap f (Step h c) = Step h (fmap f c)

To get the result of a single page, we provide the |runPage| function:

> runPage :: Page i o -> i -> NextPage -> Result o
> runPage (Fun f)           i np = Done (f i)
> runPage (Form msg parse)  _ np = Step (makeForm msg) (Cont () (runForm msg parse))
> runPage (Display msg)     i np = Step (continue (msg i) np "Continue") (Cont () (fun (const ())))
> runPage (Link  s)         i np = Step (continue X.noHtml np s) (Cont () (fun (const ())))


To run a |Form|, we parse the request, and if it is a |Failure|, we restart the
flow. Instead of rendering the form as usual, we also add the error messages.
If the form is entered correctly, we can return the value.
The function |fromSuccess| converts a |Failing a| into an |a|.

> choice :: (a -> Bool) -> Web a b -> Web a b -> Web a b
> choice f l r = proc x -> if f x then l -< x else r -< x

> runForm :: X.Html -> (RequestBody -> Failing b) -> Web () b
> runForm formHtml parse  = start
>  where start =    Req 
>              >>>  fun parse
>              >>>  choice isFailure
>                   retry 
>                   (fun fromSuccess)
>        retry = display showFormWithError `Seq` start
>        showFormWithError (Failure msgs) = makeForm (X.toHtml msgs X.+++ formHtml)

%if False

> continue :: X.HTML x => x -> NextPage -> String -> X.Html
> continue x np linkText = x X.+++ X.br X.+++ (ahref np (X.toHtml linkText))

> ahref url text = X.anchor X.! [X.href url] X.<< text

> fromFailure (Failure x) = x
> fromSuccess (Success x) = x

> isFailure (Failure x) = True
> isFailure _           = False

> type Form a = Formlets.XHtmlForm Identity a



> runForm' :: Form a -> (X.Html, RequestBody -> Failing a)
> runForm' f = let (_, Identity html, _) = Formlets.runFormState [] f
>                  parse env = x where (Identity x, _, _) = Formlets.runFormState (map (fmap Left) env) f
>              in (html, parse)

%endif

Handling a request is simple:

> handleRequest :: Web i o -> i -> NextPage -> RequestBody -> Result o
> handleRequest (Single page) inp np body = runPage page inp np
> handleRequest (Req)         inp np body = Done body
> handleRequest (Seq    l r)  inp np body = case handleRequest l inp np body of
>   Done res              -> handleRequest r res np body
>   Step page (Cont i c)  -> Step page (Cont i (c `Seq` r))
> handleRequest (First l) (i1,i2) np body = case handleRequest l i1 np body of
>     Done res              -> Done (res, i2)
>     Step page (Cont i c)  -> Step page (Cont (i,i2) (First c))
> handleRequest (Choice l) (Left inp)  np body = fmap Left $ handleRequest l inp np body
> handleRequest (Choice a) (Right inp) np body = Done (Right inp)



> type Env = M.Map String (Continuation ())

> run :: Env -> String -> RequestBody -> (X.Html, Env) 
> run env page reqBody = case M.lookup page env of
>                        Nothing   -> (pageNotFound, env)
>                        Just (Cont i c) ->
>                          let np = page
>                              result        = handleRequest c i np reqBody -- np?
>                              (html, cont') = handleResult np result
>                              env' = maybe (M.delete page env) (\x -> M.insert page x env) cont'
>                          in (html, env')

> type NextPage = String

> handleResult :: NextPage -> Result () -> (X.Html, Maybe (Continuation ()))
> handleResult np  (Done ())            = (X.toHtml "Done", Nothing)
> handleResult np  (Step msg cont)      = (msg, Just cont)

> pageNotFound = X.toHtml "Page not found."

> runServer :: Int -> Env ->  IO ()
> runServer p e = do
>   env <- newMVar e
>   start defaultConfig
>         (hDefaultEnv (do e' <- liftIO $ takeMVar env
>                          h <- hCurrentPath
>                          contId <- (intercalate "/". __segments) <$> hCurrentPath
>                          ps <- (map (fmap fromJust) . filter (isJust . snd)) <$> hRequestParameters "utf-8"
>                          let (html, e'') = run e' contId ps
>                          liftIO $ print (contId, ps)
>                          liftIO $ putMVar env e''
>                          response (contentType =: Just ("text/html", Just "utf-8"))
>                          send (show html)
>                      ))
>         ()

> hCurrentPath :: HttpM Request m => m Path
> hCurrentPath = request (getM (_path . asUri))


> exampleEnv = M.singleton "/arc" (Cont () arc'')

> main = runServer 8080 exampleEnv

> 
> arc :: Web () ()
> arc =      input 
>     &&&   link "Click Here"
>     >>>   display (\(name, _cal) -> X.toHtml $ "Hello, " ++ name)

> arc' = proc () -> do
>    name <- input -< ()
>    link "Click here" -< ()
>    display (\n -> X.toHtml $ "Hello, " ++ n) -< name

> arc'' = proc () -> do
>    x <- input -< ()
>    link "Click here" -< ()
>    if x > (10 :: Integer)
>      then display (\n -> X.toHtml $ "Hello, " ++ show n) -< x
>      else display (const $ X.toHtml "ok.") -< ()

> class DefaultForm i  where form :: Form i

%if False

> makeForm f = X.form X.! [X.method "POST"] X.<< (f X.+++ X.submit "submit" "submit")
 
> instance DefaultForm String  where form = Formlets.input Nothing
> instance DefaultForm Integer where form = Formlets.inputInteger Nothing
> instance (DefaultForm a, DefaultForm b) => DefaultForm (a,b) where 
>   form = (,) <$> form <*> form
 
> instance X.HTML Integer where toHtml = X.toHtml . show
 
> instance Applicative Identity where pure = return; (<*>) = ap;

%endif
 

% 
% We now proceed to the serialization of a |Web| value. In order to do that,
% we will first convert a recursive |Web| value into an explicit graph with
% observable sharing. This is done using the \library{data-reify} library (TODO cite) 
% from Gill (TODO cite). To see how this works, we can
% take a look at a simple example for lists. Consider the following two mutually
% recursive expressions:
% 
% \begin{spec}
% data List a = Nil | Cons a (List a)
% x = Cons 'a' y
% y = Cons 'b' x
% \end{spec}
% 
% If we try to inspect the structure of this expression, we get an infinite value.
% In order to work with \library{data-reify}, we first change our data-structure
% to its \emph{pattern-functor} (TODO cite multirec):
% 
% \begin{spec}
% newtype Fix a     = In {out :: a (Fix a)}
% data PF_List a r  = NilF | ConsF a r deriving Show
% type List' a      = Fix (PF_List a)
% \end{spec}
% 
% Now we can redefine |x| and |y| using our new datatype:
% 
% \begin{spec}
% x' = In (ConsF 'a' y')
% y' = In (ConsF 'b' x')
% \end{spec}
% 
% If we make the type |PF_List| an instance of |Traversable|, we can reify the
% recursive value |x'|, yielding the following result:
% 
% \begin{spec}
% let [(1,ConsF 'a' 2),(2,ConsF 'b' 1)] in 1
% \end{spec}
% 
% The result is a \emph{finite} list of key/value pairs, where each key represents
% a node in the graph. The recursive positions have been replaced by a reference
% to such a key.  Because our program is always finite, it should always be
% possible to build such a graph. 
% 
% If we explicitly encode the structure of an arrow-based computation, we can build a finite representation of our program using this technique. We can number all the nodes and use them as code pointers. Because the environment is always explicitly passed on between every bind, we can restore a continuation from disk, given an input for an arrow and a code pointer.
% 
% \subsection{Conclusion}
% 
% We have extend our examples to be a little more complex, such as using dynamic choice. Without arrow-notation, this quickly becomes very inconvenient. Using arrow-notation, the examples look a bit simpler but are still a lot more complex than the monadic notation. It is a matter of taste, but arrow notation is awkward and not a good match for web programming.
