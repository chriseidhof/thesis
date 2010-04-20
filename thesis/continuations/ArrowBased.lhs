%if False

> {-# LANGUAGE GADTs, ExistentialQuantification, TypeSynonymInstances,
> FlexibleContexts, PackageImports, Arrows #-}
> module ArrowBased where

> import "mtl" Control.Monad.Identity (Identity (..))
> import "transformers" Control.Monad.Trans
> import Control.Applicative
> import Control.Applicative.Error (Failing (..))
> import Control.Arrow
> import Control.Category
> import Control.Concurrent.MVar
> import Control.Monad.Reader hiding (liftIO)
> import Data.List (intercalate)
> import Data.Maybe (isJust, fromJust)
> import Data.Record.Label
> import Network.Protocol.Http
> import Network.Protocol.Http.Data
> import Network.Protocol.Uri
> import Network.Protocol.Uri.Data
> import Network.Salvia.Handlers
> import Network.Salvia.Impl.Config
> import Network.Salvia.Impl.Server
> import Network.Salvia.Interface
> import Prelude as Prelude hiding ((.))
> import qualified Data.ByteString.Lazy.Char8 as B
> import qualified Data.Map as M
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as Formlets


%endif

As we have seen in the previous section, web continuations can be expressed as
monads.
However, because the monadic approach stores functions inside the |Web| and
|Result| datatypes, it is not possible to serialize these datatypes.
If we change our library to an Arrow-based interface
\cite{hughes2000generalising}, we can solve this. We first show some
examples, then implement the library in section \ref{sec:arrowimpl},
and finally discuss how to serialize these continuations in \ref{sec:arrowserial}.

First, we show some example programs using our library. For the API used in
these examples see section \ref{sec:arrowinterface}.
The solution to the Arc challenge stated the introduction of this chapter is
written down in the following way:

> arc :: Web () ()
> arc =     input 
>     &&&   link "Click Here"
>     >>>   display (\x -> X.toHtml $ "Hello, " ++ fst x)

Using arrow notation \cite{paterson2001new}, we can write down our example in a
style that is reminiscent of monadic do-notation. Arrow notation can save a lot
of code, especially when dealing with variables that are used multiple times.
With normal arrows, these variables have to be explicitly threaded each time,
and using arrow notation all the threading is implicit.

> arc' = proc () -> do
>    name <- input                              -< ()
>    link "Click here"                          -< ()
>    display (\n -> X.toHtml $ "Hello, " ++ n)  -< name

The difference with monadic do-notation is the |-<| symbol, which denotes the
input of the function.
Compared to writing arrows using standard combinators, arrow notation becomes
especially useful when constructing larger programs.
When desugared, the following programming is quite large.
However, using arrow notation, it is almost as simple as its monadic
counterpart.

> arcExtended = proc () -> do
>    x     <- input      -< ()
>    name  <- input      -< ()
>    link "Click here"   -< ()
>    display X.toHtml    -< (name :: String)
>    if x > (42 :: Integer)
>       then display (\n -> X.toHtml $ "Large number: " ++ show n)  -< x
>       else display (const $ X.toHtml "Small.")                    -< ()

\subsection{The library implementation}
\label{sec:arrowimpl}

The difference between monads and arrows has been a topic of research since the
introduction of arrows \cite{hughes2000generalising, lindley2008idioms}.
However, for our purposes, we focus on two important aspects of arrows:

\begin{itemize}
\item Arrows are \emph{explicit in their environment}. The environment that an
arrow uses is always explicit as its input parameter.
This means that users of an arrow-based interface have to explicitly define
inputs and outputs of each arrow, but this problem is largely solved by using
arrow notation.
\item We can represent our |Web| structure \emph{without using functions as
continuations}. Because arrows are explicit about their environment, we can
store the environment and a \emph{trace} that describes how we got the current
state. From such an environment and a trace we can construct the continuation
after a program restart. A similar approach is taken in WASH
\cite{thiemann2002wash}.
\end{itemize}


\begin{figure}
\begin{spec}
class Category a => Arrow a where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  (&&&) :: a b c -> a b c' -> a b (c, c')
\end{spec}

\caption{The Arrow typeclass}
\label{fig:Arrow}

\end{figure}

We now redefine the |Web| datatype in such a way that we can make it an
instance of the |Arrow| typeclas. The |Arrow| typeclass is defined in figure
\ref{fig:Arrow}, and the minimal definition consists of |arr| and |first|. As we
can see, any |Arrow| instance needs to be of kind |* -> * -> *|, and the |Web|
datatype from the previous section is of kind |* -> *|.

Therefore, we add an extra type-parameter |i|, which captures the input of a
|Web| computation, or in other words: its environment.
The |o| type-parameter indicates the result of running a |Web| computation.
Note that there are no functions stored in the |Web| type itself, which is
essential for serialization, as we show later on.

> data Web i o where

The |Single| constructor indicates a single |Page| that is shown to the user.

>   Single  :: Page i o -> Web i o

The |Req| is a constructor that produces the |RequestBody| for a specific
request. It is needed to provide global access to the |RequestBody| throughout a
computation.

>   Req     :: Web () RequestBody

To compose two |Web| computations, we provide the |Seq| constructor.
 
>   Seq     :: Web a b -> Web b c -> Web a c

When we want to thread a value through a computation, we can use the |First|
constructor. It takes a computation, and adds an extra |c| value to the input and
the output.
When running the |Web| computation, the |c| input does not change.

>   First   :: Web a b -> Web (a, c) (b, c)

Finally, we provide a constructor |Choice| for making choices. If the input is
a type |a| wrapped in a |Left| constructor, the |Web| action is executed. If the
input is a |c| value wrapped in the |Right| constructor, the output will be a
value of type |Right c| too.

>   Choice  :: Web a b -> Web (Either a c) (Either b c)

We have made |Web| an instance for the |Functor|, |Arrow|, |ArrowChoice| and
|Category| type-classes, as seen in Figure \ref{fig:webinstances}.

\begin{figure}

> instance Functor (Web i)  where fmap = flip Seq . arr
> instance ArrowChoice Web  where left = Choice
>
> instance Arrow Web        where
>   arr   = Single . Fun
>   first = First
>
> instance Category Web     where
>   id  = arr Prelude.id
>   (.) = flip Seq

\caption{Instances for common type-classes}
\label{fig:webinstances}

\end{figure}

The |Page| datatype from the previous section is changed in the same way. We add
an extra parameter |i| that captures the input of a page. Furthermore, we add a
constructor |Fun| that can contain any Haskell function.

> data Page i o where
>   Fun      :: (i -> o)                              -> Page i o
>   Form     :: X.Html -> (RequestBody -> Failing o)  -> Page a o
>   Display  :: (a -> X.Html)                         -> Page a ()
>   Link     :: String                                -> Page a ()

We also provide smart constructors that lift a |Page| type into the |Web| type:

> input    :: DefaultForm f => Web () f
> display  :: (i -> X.Html) -> Web i ()
> link     :: String -> Web i ()


Now we can define the |Result| datatype, which is very similar to the |Result|
datatype from the previous section. A |Result| can either be completely done,
which is indicated by the |Done| constructor, or it can yield some HTML and a
continuation.

> data Result o where
>   Done    :: o -> Result o
>   Step    :: X.Html -> Continuation o -> Result o

The |Continuation| is not just a |Web i o| value, but also stores the input |i|.
We can wrap this in an existential type.

> data Continuation o = forall i . Cont i (Web i o)

%if False

> instance Functor Continuation where
>   fmap f (Cont i w) = Cont i (fmap f w)
> 
> instance Show (Continuation o) where show (Cont i w) = show w


> instance Functor Result where
>   fmap f (Done x) = Done (f x)
>   fmap f (Step h c) = Step h (fmap f c)

%endif

To get the result of a single page, we provide the |runPage| function, which is
again very similar to the |runPage| function in the previous section.

> runPage :: Page i o -> i -> NextPage -> Result o
> runPage (Fun f)           i np = Done  (f i)
> runPage (Form msg parse)  _ np = Step  (makeForm msg) 
>                                        (returnCont $ runForm msg parse)
> runPage (Display msg)     i np = Step  (continue (msg i) np "Continue") 
>                                        noResult
> runPage (Link  s)         i np = Step  (continue X.noHtml np s)
>                                        noResult

The helper functions |noResult| and |returnCont| build simple continuations:

> noResult :: Continuation ()
> noResult = returnCont (arr (const ()))

> returnCont :: Web () o -> Continuation o
> returnCont = Cont ()

To run a |Form|, we parse the request, and if it is a |Failure|, we restart the
flow. Instead of rendering the form as usual, we also add the error messages.
If the form is entered correctly, we can return the value.
The function |fromSuccess| converts a |Failing a| into an |a|.


> runForm :: X.Html -> (RequestBody -> Failing b) -> Web () b
> runForm formHtml parse  = start
>  where start =    Req 
>              >>>  arr parse
>              >>>  choice isFailure
>                   retry 
>                   (arr fromSuccess)
>        retry = display showFormWithError `Seq` start
>        showFormWithError (Failure msgs) = makeForm (X.toHtml msgs X.+++ X.br X.+++ formHtml)

> choice :: (a -> Bool) -> Web a b -> Web a b -> Web a b
> choice f l r = proc x -> if f x then l -< x else r -< x

Now we can define the function |handleRequest|, which takes a |Web| value and
its input, a fresh URL that is used as link to the next page and a
|RequestBody|. It produces a |Result|, which is either |Done| or a
continuation.

> handleRequest :: Web i o -> i -> NextPage -> RequestBody -> Result o

The cases for |Req| and |Single| are straightforward:

> handleRequest (Req)         inp np body = Done body
> handleRequest (Single page) inp np body = runPage page inp np

To handle a |Seq|, we first handle the request for the left part of the
sequence. If it returns with a |Done| value, we can recursively continue with the right
part.
However, if we find a |Step|, we restructure the |Step| to include the right
part of the sequence in the continuation of the |Step|:

> handleRequest (Seq    l r)  inp np body = case handleRequest l inp np body of
>   Done res              -> handleRequest r res np body
>   Step page (Cont i c)  -> Step page (Cont i (c `Seq` r))

The |First| constructor threads values and is defined in a straightforward way:

> handleRequest (First l) (i1,i2) np body = case handleRequest l i1 np body of
>     Done res              -> Done (res, i2)
>     Step page (Cont i c)  -> Step page (Cont (i,i2) (First c))

Finally, for the |Choice| constructor we pattern-match on the input type, which
is an |Either| value:

> handleRequest (Choice a) (Left   inp) np body = Left <$> handleRequest a inp np body
> handleRequest (Choice a) (Right  inp) np body = Done (Right inp)

The rest of the functions are very similar to their monadic counterparts, and
can be found in the code accompanying this thesis. To be complete, we have
provided the library interface in section \ref{sec:arrowinterface}.

%if False

> type RequestBody = [(String,String)]

> input = Single (uncurry Form (runForm' form))
> display = Single . Display
> link = Single . Link

> instance Show (Web i o) where
>   show (Single s) = show s
>   show Req        = "Req"
>   show (Seq a b)  = show a ++ " >>> " ++ show b
>   show (First f)      = "First (" ++ show f ++ ")"
>   show (Choice a)    = "Choice (" ++ show a ++ ")"

> instance Show (Page i o) where
>   show (Fun f) = "Fun"
>   show (Form f _) = "Form " ++ show f
>   show (Display f) = "Display"
>   show (Link l) = "link: " ++ l

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


> type Env = M.Map String (Continuation ())

> run :: Env -> String -> RequestBody -> (X.Html, Env) 
> run env page reqBody = case M.lookup page env of
>                        Nothing   -> (pageNotFound, env)
>                        Just (Cont i c) ->
>                          let np = page
>                              result        = handleRequest c i np reqBody
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


> exampleEnv = M.singleton "/arc" (Cont () arcExtended)

> main = runServer 8080 exampleEnv


> class DefaultForm i  where form :: Form i

> makeForm f = X.form X.! [X.method "POST"] X.<< (f X.+++ X.submit "submit" "submit")
 
> instance DefaultForm String  where form = Formlets.input Nothing
> instance DefaultForm Integer where form = Formlets.inputInteger Nothing
> instance (DefaultForm a, DefaultForm b) => DefaultForm (a,b) where 
>   form = (,) <$> form <*> form
 
> instance X.HTML Integer where toHtml = X.toHtml . show
 
> instance Applicative Identity where pure = return; (<*>) = ap;

%endif
