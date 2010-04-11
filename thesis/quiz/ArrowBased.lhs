%if False

> {-# LANGUAGE GADTs, ExistentialQuantification, TypeSynonymInstances,
> FlexibleContexts, PackageImports, Arrows #-}
> module ArrowBased where

> import "mtl" Control.Monad.Identity (Identity (..))
> import "transformers" Control.Monad.Trans
> import Control.Applicative hiding (many)
> import Control.Applicative.Error (Failing (..))
> import Control.Arrow
> import Control.Category
> import Control.Concurrent.MVar
> import Control.Monad.Reader hiding (liftIO, forever)
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
> import Routing
> import Prelude as Prelude hiding ((.))
> import qualified Data.ByteString.Lazy.Char8 as B
> import qualified Data.Map as M
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as Formlets
> import System.Random


%endif

As we have seen in the previous section, web continuations can be expressed as
monads.
However, because the monadic approach stores functions inside the |WebT m| and
|Result| datatypes, it is not possible to serialize these datatypes.
If we change our library to an Arrow-based interface
\cite{hughes2000generalising}, we can solve this. We will first show some
examples, then implement the library in section \ref{sec:arrowimpl},
and finally discuss how to serialize these continuations in \ref{sec:arrowserial}.

First, we will show some example programs using our library. For the API used in
these examples see section \ref{sec:arrowinterface}.
The solution to the Arc challenge stated the introduction of this chapter is
written down in the following way:

> arc :: Monad m => WebT s m () ()
> arc =     input 
>     &&&   link "Click Here"
>     >>>   display (\x -> X.toHtml $ "Hello, " ++ fst x)

Using arrow notation \cite{paterson2001new}, we can write down our example in a
style that is reminiscent of monadic do-notation. Arrow notation can save a lot
of code, especially when dealing with variables that are used multiple times.
With normal arrows, these variables have to be explicitly threaded each time,
and using arrow notation all the threading is implicit.

> arc' :: Monad m => WebT s m () ()
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

> arcExtended :: Monad m => WebT s m () ()
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
\item We can represent our |WebT m| structure \emph{without using functions as
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

We will now redefine the |WebT m| datatype in such a way that we can make it an
instance of the |Arrow| typeclas. The |Arrow| typeclass is defined in figure
\ref{fig:Arrow}, and the minimal definition consists of |arr| and |first|. As we
can see, any |Arrow| instance needs to be of kind |* -> * -> *|, and the |WebT m|
datatype from the previous section is of kind |* -> *|.

Therefore, we add an extra type-parameter |i|, which captures the input of a
|WebT m| computation, or in other words: its environment.
The |o| type-parameter indicates the result of running a |WebT m| computation.
Note that there are no functions stored in the |WebT m| type itself, which is
essential for serialization, as we will see later on.

> -- type Web i o = WebT IO i o

> data WebT s m i o where

The |Single| constructor indicates a single |Page| that is shown to the user.

>   Single  :: Page m i o -> WebT s m i o

The |Req| is a constructor that produces the |RequestBody| for a specific
request. It is needed to provide global access to the |RequestBody| throughout a
computation.

>   Req     :: WebT s m () RequestBody

To access the state, we provide a constructor |St|:

>   St      :: WebT s m () s

To compose two |Web| computations, we provide the |Seq| constructor.
 
>   Seq     :: WebT s m a b -> WebT s m b c -> WebT s m a c

When we want to thread a value through a computation, we can use the |First|
constructor. It takes a computation, and adds an extra |c| value to the input and
the output.
When running the |WebT| computation, the |c| value will be passed around
unchanged.

>   First   :: WebT s m a b -> WebT s m (a, c) (b, c)

Finally, we provide a constructor |Choice| for making choices. We will see how
to use this later on. \todo{Explain better.}

>   Choice  :: WebT s m a b -> WebT s m (Either a c) (Either b c)

We have made |WebT m| an instance for the |Functor|, |Arrow|, |ArrowChoice| and
|Category| type-classes, as seen in Figure \ref{fig:webinstances}.

\begin{figure}

> instance Monad m => Functor (WebT s m i)    where fmap = flip Seq . arr
> instance Monad m => ArrowChoice (WebT s m)  where left = Choice
>
> instance Monad m => Arrow (WebT s m)        where
>   arr f = Single (Fun (\x -> return (f x)))
>   first = First
>
> instance Monad m => Category (WebT s m)     where
>   id  = arr Prelude.id
>   (.) = flip Seq

> liftAM :: Monad m => (i -> m o) -> WebT s m i o
> liftAM f = Single (Fun (\x -> (f x)))

> liftAM' :: Monad m => m o -> WebT s m i o
> liftAM' f = Single (Fun (const f))

\caption{Instances for common type-classes}
\label{fig:webinstances}

\end{figure}

The |Page| datatype from the previous section is changed in the same way. We add
an extra parameter |i| that captures the input of a page. Furthermore, we add a
constructor |Fun| that can contain any Haskell function.

> data Page m i o where
>   Fun      :: (i -> m o)                          -> Page m i o
>   Form     :: (i -> (X.Html, (RequestBody -> o))) -> Page m i o
>   Display  :: (a -> X.Html)                       -> Page m a ()
>   Link     :: String                              -> Page m a ()

We also provide smart constructors that lift a |Page| type into the |WebT m| type:

> input    :: (Monad m, DefaultForm f) => WebT s m () f
> display :: (X.HTML h) => (i -> h) -> WebT s m i ()
> link     :: String -> WebT s m i ()

Now we can define the |Result| datatype, which is very similar to the |Result|
datatype from the previous section. A |Result| can either be completely done,
which is indicated by the |Done| constructor, or it can yield some HTML and a
continuation.

> data Result s m o where
>   Done      :: o -> Result s m o
>   Step      :: X.Html -> Continuation s m o -> Result s m o

The |Continuation| is not just a |WebT s m i o| value, but also stores the input |i|.
We can wrap this in an existential type.

> data Continuation s m o = forall i . Cont i (WebT s m i o)

%if False

> instance Monad m => Functor (Continuation s m) where
>   fmap f (Cont i w) = Cont i (fmap f w)
> 
> instance Show (Continuation s m o) where show (Cont i w) = show w


> instance Monad m => Functor (Result s m) where
>   fmap f (Done x)   = Done (f x)
>   fmap f (Step h c) = Step h (fmap f c)

%endif

To get the result of a single page, we provide the |runPage| function, which is
again very similar to the |runPage| function in the previous section.

> runPage :: Monad m => Page m i o -> i -> NextPage -> m (Result s m o)
> runPage (Fun f)           i np = liftM Done $ f i
> runPage (Form f )         i np = let (msg, parse) = f i in
>                                   return $ Step  (makeForm np msg) 
>                                        (        returnCont $ runForm msg parse)
> runPage (Display msg)     i np = return $ Step  (continue (msg i) np "Continue") 
>                                                 noResult
> runPage (Link  s)         i np = return $ Step  (continue X.noHtml np s)
>                                                 noResult

The helper functions |noResult| and |returnCont| build simple continuations:

> noResult :: Monad m => Continuation s m ()
> noResult = returnCont (arr (const ()))

> returnCont :: Monad m => WebT s m () o -> Continuation s m o
> returnCont = Cont ()

To run a |Form|, we parse the request, and if it is a |Failure|, we restart the
flow. Instead of rendering the form as usual, we also add the error messages.
If the form is entered correctly, we can return the value.
The function |fromSuccess| converts a |Failing a| into an |a|.


> runForm :: Monad m => X.Html -> (RequestBody -> b) -> WebT s m () b
> runForm formHtml parse  = start
>  where start =    Req 
>              >>>  arr parse

> choice :: Monad m => (a -> Bool) -> WebT s m a b -> WebT s m a b -> WebT s m a b
> choice f l r = proc x -> if f x then l -< x else r -< x

Now we can define the function |handleRequest|, which takes a |WebT m| value and
its input, a fresh URL that is used as link to the next page and a
|RequestBody|. It will produce a |Result|, which is either |Done| or a
continuation.

> handleRequest :: Monad m => WebT s m i o -> s -> i -> NextPage -> RequestBody -> m (Result s m o)

The cases for |Req| and |Single| are straightforward:

> handleRequest (Req)         s inp np body = return $ Done body
> handleRequest St            s inp np body = return $ Done s
> handleRequest (Single page) s inp np body = runPage page inp np

To handle a |Seq|, we first handle the request for the left part of the
sequence. If it returns with a |Done| value, we can recursively continue with the right
part.
However, if we find a |Step|, we restructure the |Step| to include the right
part of the sequence in the continuation of the |Step|:

> handleRequest (Seq l r) s inp np body = do 
>  x <- handleRequest l s inp np body
>  case x of
>    Done res              -> handleRequest r s res np body
>    Step page (Cont i c)  -> return $ Step page (Cont i (c `Seq` r))

The |First| constructor threads values and is defined in a straightforward way:

> handleRequest (First l) s (i1,i2) np body = do
>  x <- handleRequest l s i1 np body
>  case x of
>     Done res              -> return $ Done (res, i2)
>     Step page (Cont i c)  -> return $ Step page (Cont (i,i2) (First c))

Finally, for the |Choice| constructor we pattern-match on the input type, which
is an |Either| value:

> handleRequest (Choice a) s (Left   inp) np body = do 
>   x <- handleRequest a s inp np body
>   return $ fmap Left x
> handleRequest (Choice a) s (Right  inp) np body = return $ Done (Right inp)

The rest of the functions are very similar to their monadic counterparts, and
can be found in the code accompanying this thesis. To be complete, we have
provided the library interface in section \ref{sec:arrowinterface}.

%if False

> type RequestBody = [(String,String)]

> inputMany :: (Monad m, DefaultForm f) => WebT s m () [f]
> inputMany = input' >>> choice isFailure
>                        (arr (const []))
>                        (proc (Success x) -> do
>                          xs <- inputMany -< ()
>                          returnA -< (x:xs)
>                        )

> forever :: Monad m => WebT s m a a -> WebT s m a a
> forever w = w >>> forever w

> input' :: (Monad m, DefaultForm f) => WebT s m () (Failing f)
> input' = form' form

> form' :: Monad m => Form a -> WebT s m () (Failing a)
> form' f = Single (Form $ const (formHtml, parse))
>  where (formHtml, parse) = runForm' f

> input = form'' form

> many :: Monad m => WebT s m a b -> WebT s m [a] [b]
> many w = proc inp -> do
>   case inp of
>     []     -> (returnA -< [])
>     (x:xs) -> (do x' <- w -< x
>                   xs'<- many w -< xs
>                   returnA -< (x':xs')
>               )

> form'' :: Monad m => Form a -> WebT s m () a
> form'' f = form''' (const f)

> form''' :: Monad m => (a -> Form b) -> WebT s m a b
> form''' f = start
>  where -- (formHtml, parse) = runForm' f
>        start = proc x -> do
>             res <- (Single $ Form (\x -> runForm' $ f x)) -< x
>             if isFailure res
>               then do display showError -< res
>                       start -< x
>               else arr fromSuccess -< res
>        showError (Failure msgs) = X.toHtml msgs

> display f = Single (Display (\x -> X.toHtml $ f x))
> link = Single . Link

> instance Show (WebT s m i o) where
>   show (Single s) = show s
>   show Req        = "Req"
>   show (Seq a b)  = show a ++ " >>> " ++ show b
>   show (First f)      = "First (" ++ show f ++ ")"
>   show (Choice a)    = "Choice (" ++ show a ++ ")"

> instance Show (Page m i o) where
>   show (Fun f) = "Fun"
>   show (Form  _) = "Form "
>   show (Display f) = "Display"
>   show (Link l) = "link: " ++ l

> continue :: X.HTML x => x -> NextPage -> String -> X.Html
> continue x np linkText = x X.+++ X.br X.+++ (ahref np (X.toHtml linkText))

> ahref url text = X.anchor X.! [X.href $ u url] X.<< text

> fromFailure (Failure x) = x
> fromSuccess (Success x) = x

> isFailure (Failure x) = True
> isFailure _           = False

> type Form a = Formlets.XHtmlForm Identity a



> runForm' :: Form a -> (X.Html, RequestBody -> Failing a)
> runForm' f = let (_, Identity html, _) = Formlets.runFormState [] f
>                  parse env = x where (Identity x, _, _) = Formlets.runFormState (map (fmap Left) env) f
>              in (html, parse)


> data Env s m = Env s (M.Map String (Continuation s m ()))

> run :: Monad m => Env s m -> String -> RequestBody -> m (X.Html, Env s m) 
> run env@(Env s m) page reqBody = case M.lookup page m of
>   Nothing   -> return (pageNotFound, env)
>   Just (Cont i c) -> do
>     let np = page
>     result        <- handleRequest c s i np reqBody
>     (html, cont') <- handleResult np result
>     let m' = maybe (M.delete page m) (\x -> M.insert page x m) cont'
>     return (html, Env s m')

> type NextPage = String

> handleResult :: Monad m => NextPage -> Result s m () -> m (X.Html, Maybe (Continuation s m ()))
> handleResult np  (Done x )            = return (X.toHtml "Done", Nothing)
> handleResult np  (Step msg cont)      = return (msg, Just cont)

> pageNotFound = X.toHtml "Page not found."

> randomChar = getStdRandom (randomR ('a','z'))

> randomString :: IO String
> randomString = replicateM 40 randomChar

> addCont :: Env s m -> String -> Continuation s m () -> Env s m
> addCont (Env s mp) nm c = Env s $ M.insert nm c mp

> runServer :: ToURL route => Int -> X.Html -> (route -> Continuation s IO ()) -> Env s IO ->  IO ()
> runServer p index handle e = do
>   env <- newMVar e
>   start defaultConfig
>         (hDefaultEnv (do path <- (tail . __segments) <$> hCurrentPath
>                          html <- if path == [] then return index else do
>                            e' <- liftIO $ takeMVar env
>                            liftIO $ print path
>                            (e'',contId) <- liftIO $ if (length path == 2 && head path == "c")
>                               then return (e', head (tail path))
>                               else (case fmap handle (fromURL path) of
>                                      Nothing -> return (e', "")
>                                      Just p  -> do newId <- randomString
>                                                    let eNew = addCont e' newId p
>                                                    return (eNew, newId))
>                            ps <- (map (fmap fromJust) . filter (isJust . snd)) <$> hRequestParameters "utf-8"
>                            (html, e''') <- liftIO $ run e'' contId ps
>                            liftIO $ print (contId, ps)
>                            liftIO $ putMVar env e'''
>                            return html
>                          response (contentType =: Just ("text/html", Just "utf-8"))
>                          send (show html)
>                      ))
>         ()

> hCurrentPath :: HttpM Request m => m Path
> hCurrentPath = request (getM (_path . asUri))


> class DefaultForm i  where form :: Form i

> u x = "/c/" ++ x

> makeForm np f = X.form X.! [X.method "POST", X.action (u np)] X.<< (f X.+++ X.submit "submit" "submit")
 
> instance DefaultForm String  where form = Formlets.input Nothing
> instance DefaultForm Integer where form = Formlets.inputInteger Nothing
> instance (DefaultForm a, DefaultForm b) => DefaultForm (a,b) where 
>   form = (,) <$> form <*> form
 
> instance X.HTML Integer where toHtml = X.toHtml . show
 
> instance Applicative Identity where pure = return; (<*>) = ap;

%endif
