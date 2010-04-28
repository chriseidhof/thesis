%if False

> {-# LANGUAGE GADTs, TypeSynonymInstances #-}
> module Monadic where

> import Control.Applicative
> import Control.Applicative.Error (Failing (..))
> import Control.Monad (ap)
> import Control.Monad.Identity (Identity (..))
> import Control.Monad.Reader
> import Control.Concurrent.MVar
> import Control.Monad.Trans
> import Happstack.Server.SimpleHTTP hiding (Web)
> import Text.XHtml.Strict (toHtml)
> import qualified Data.ByteString.Lazy.Char8 as B
> import qualified Data.Map as M
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as Formlets
> import qualified Happstack.Server.SimpleHTTP as H
 
The |NextPage| type is simply a |String| value.
 
> type NextPage = String

%endif

In this section we build a monadic continuation-based
web programming library. The full interface of the library is given in section
\ref{sec:monadinterface},
and is inspired by iTasks \cite{plasmeijeriTasks}, which is a
library in the Clean language.

When a web server receives a request for a page, both the URL and the
|RequestBody| are sent. 
If a form is submitted, the |RequestBody| contains
the key/value pairs for each form field. Otherwise it is empty:
 
> type RequestBody = [(String,String)]
 
The |Page| datatype defines atomic pages. The |Form| constructor constructs pages that consist of a form:
the first parameter for |Form| is the rendering of the form and the
second parameter is the parsing function, which is evaluated only after the user has submitted
the form. The result of the parsing function is wrapped in the |Failing|
datatype, which either contains the result of a succesful parse or a list of
error messages.
The |Display| constructor displays a piece of HTML.
Finally, we include a |Link| constructor that displays just a link.
Our choice of |Page| constructors might seem a bit limited, but instead of a |Page|
datatype we could have provided a |Page| typeclass, where programmers can give
their own instances if something is missing.
 
> data Page a where
>   Form     :: X.Html -> (RequestBody -> Failing a)  -> Page a
>   Display  :: X.Html                                -> Page ()
>   Link     :: String                                -> Page ()
 
A web continuation is a function that maps a |RequestBody| to a result. 
We will explain the |NextPage| type later.
 
> newtype Web a = Web {runWeb :: NextPage -> RequestBody -> Result a}
 
The |Result| datatype contains the result of running a web continuation. When
the interaction is completely finished, it returns the |Done| constructor.
Alternatively, a problem might have occured, resulting in error message and a new continuation.
Finally, in most common case, the result is a new page to be displayed, with a
continuation (the |Web a| part), which is expressed using the |Step| constructor.
 
> data Result a  =  Done a
>                |  Problem  String  (Web a)
>                |  Step     X.Html  (Web a)

 
From a single page we can compute a function that takes a request and
produces a |Result|. The |runPage| function takes the |Page| is its first
argument. The second argument is of type |NextPage|, which contains a fresh URL
that can be used to produce a hyperlink to the next page. The final argument is
a |RequestBody|, which contains the submitted form data:
 
> runPage :: Page a ->  NextPage -> RequestBody -> Result a

For the |Display| constructor, we display the message using the |continue|
function, which we define later.
For the second field of the |Step| constructor, which has type |Web a|, we
return a unit value.  The |Link| constructor follows a similar pattern.

> runPage   (Display msg)    np _  = Step (continue msg      np "Continue")
>                                         (return ())
> runPage   (Link msg)       np _  = Step (continue X.noHtml np msg)
>                                         (return ())

The case for |Form| is more interesting: first it displays the HTML of the form
using the |makeForm| function. However, the second field of the |Step|
constructor is now initialized with a continuation that parses the request. The
|web| function lifts a |Page| value to a |Web| value, and |formResult| shows the
form repeatedly until there is a succesful parse.

> runPage f@(Form msg parse) np _  = Step (makeForm msg)
>                                         (Web (const (formResult (web f) . parse)))
 
We can make the |Web| type an instance of the |Monad| typeclass. The return function
ignores the |NextPage| and |RequestBody|, and returns a |Done| result:
 
> instance Monad Web where
>   return   = Web . const . const . Done

To combine two |Web| values, we construct a new |Web| value. We first run the
left operand |l|, and pattern-match on the result. If the result is |Done|, we can
continue with the right operand.
If we display something using the |Step| or |Problem| constructor, we combine the
second field of the step constructor (which is of type |Web| a) with the right
operand |r|.

>   l >>= r  = Web $ \np req -> case runWeb l np req of
>                Done x          -> runWeb (r x) np req
>                Step msg l'     -> Step     msg  (l' >>= r) 
>                Problem msg l'  -> Problem  msg  (l' >>= r)

At this point, we have defined a library for expressing web interactions, using the basic
elements in |Page| and the monad instance we can already express advanced
interactions. However, to run them we need to interpret |Web| values as programs
and execute them on a web server.
To be able to store continuations on the server, we keep track of an environment |Env| that maps a
URL to a continuation:

> type Env = M.Map String (Web ())

Given the environment |Env|, a URL and a request body, we can produce Html and a new,
changed environment. The |run| functions looks up the URL in the environment,
and if it is found, it runs the continuation. That yields a value |result|. From
the |result| we can derive the HTML that needs to be displayed, and the next
continuation.
Finally, we update the environment and return the HTML with the
new environment.
 
> run :: Env -> String -> RequestBody -> (X.Html, Env) 
> run env page reqBody = case M.lookup page env of
>                        Nothing   -> (pageNotFound, env)
>                        Just cont ->
>                          let np = "/" ++ page
>                              result        = runWeb cont np reqBody
>                              (html, cont') = handleResult np result
>                              env' = maybe (M.delete page env) (\x -> M.insert page x env) cont'
>                          in (html, env')
 
The |handleResult| calculates the HTML to be displayed, given a fresh URL and a
|Result| value. Additionally, it calculates the next continuation. When the
|Result| value is |Done|, there is no next continuation. 
 
> handleResult :: NextPage -> Result () -> (X.Html, Maybe (Web ()))
> handleResult np  (Done ())            = (toHtml "Done", Nothing)
> handleResult np  (Step msg cont)      = (msg, Just cont)
> handleResult np  (Problem msg retry)  = (continue ("Problem: " ++ msg) np "Continue", Just retry)

 
To run the application, we provide helper functions that build a
Happstack\footnote{\url{http://hackage.haskell.org/package/happstack}}
application. In this application, there is one global |Env| value, which means
that continuations are per-application and not per-user.
In a final version of this library, the |Env| should be stored in a session.
 
> runServer :: Int -> Env -> IO ()
> runServer p env = do
>     serverPart <- createServerPart env
>     putStrLn $ "Running server at http://127.0.0.1:" ++ show  p
>     simpleHTTP (nullConf { port = p }) serverPart
>
> createServerPart :: Env -> IO (ServerPart Response)
> createServerPart e = do env <- newMVar e
>                         return $ ServerPartT $ handle env
> 
> handle :: MVar Env -> ReaderT Request (WebT IO) Response
> handle env = do req <- ask
>                 let contId     = foldr const "/" (rqPaths req)
>                     formInputs = map (\(k,v) -> (k, B.unpack $ inputValue v)) $ rqInputs req
>                 e <- liftIO $ takeMVar env
>                 let (html, e') = run e contId formInputs
>                 liftIO $ putMVar env e'
>                 return $ toResponse html
 
 
We have defined some smart constructors that lift a |Page| directly into the |Web| type:
 
> display  :: X.HTML h => h -> Web ()
> input    :: DefaultForm a => Web a
> link     :: String -> Web ()

%if False

> display  = web . Display . toHtml
> input    = web . uncurry Form . runForm $ form
> link     = web . Link

> pageNotFound = X.toHtml "Page not found."
 
> continue :: X.HTML x => x -> NextPage -> String -> X.Html
> continue x np linkText = x X.+++ X.br X.+++ (ahref np (toHtml linkText))
 
> add :: Integer -> Integer -> Integer
> add = (+)
 
> ahref url text = X.anchor X.! [X.href url] X.<< text
 
 
> formResult frm (Success a)  = Done a
> formResult frm (Failure xs) = Problem (unlines xs) frm
 
> web = Web . runPage

%endif

Finally, some code to abstract working with forms. This makes use of the
formlets library\footnote{\url{http://hackage.haskell.org/package/hackage}}. We
provide a class |DefaultForm| with instances for |String|, |Integer| and |(,)|. 
Although the library described above is not in any way dependent on formlets,
they provide a good abstraction and integrate easily.
 
> class DefaultForm i  where form :: Form i
> type Form a = Formlets.XHtmlForm Identity a

%if False
> makeForm f = X.form X.! [X.method "POST"] X.<< (f X.+++ X.submit "submit" "submit")
 
> runForm :: Form a -> (X.Html, RequestBody -> Failing a)
> runForm f = let (_, Identity html, _) = Formlets.runFormState [] f
>                 parse env = x where (Identity x, _, _) = Formlets.runFormState (map (fmap Left) env) f
>             in (html, parse)
 
> instance DefaultForm String  where form = Formlets.input Nothing
> instance DefaultForm Integer where form = Formlets.inputInteger Nothing
> instance (DefaultForm a, DefaultForm b) => DefaultForm (a,b) where 
>   form = (,) <$> form <*> form
 
> instance X.HTML Integer where toHtml = toHtml . show
 
> instance Applicative Identity where pure = return; (<*>) = ap;

%endif

\subsection{Serialization}

When we run our program as a process on the webserver, we can keep the continuations in-memory.
However, we would like to store the continuations on disk.
There are two main reasons for this:

\begin{itemize}

\item \emph{Scaling to multiple servers.} When a web application becomes large,
it is necessary to add more servers that handle the requests from the users.
In our approach, we cannot simply move a continuation to a different server.
As a workaround, we can add an intelligent load balancer, that always connects a
client to the same server.

\item \emph{Restart of the server.} Sometimes it is necessary to restart the
server. For example, when the code on the server has changed or when the server
needs to be rebooted. When we do not serialize our continuations, all 
state that users have is lost, and each workflow needs to be restarted.

\end{itemize}

Because our continuation type |Request -> Result ()| is a function type,
it is impossible to serialize it.
While our representation is conceptually elegant, it is not possible to preserve
it externally.

There are a couple of solutions to solve this problem, but they all involve a
large amount of work that we could not fit into this thesis.

\begin{itemize}
\item We may \emph{extend the compiler} to support the serialization of
functions.
This is the approach taken by the Clean compiler, which wraps everything in a
|Dynamic| type which stores a value along with a representation of its type.
This makes it possible to check, when we reload the function, that our code is
still type-safe.
\item We may use an explicit structure to represent our continuations.
For example, instead of a monadic
interface, we can use an \emph{arrow interface}.
We expand on this approach in section \ref{sec:arrowbased}
\item We may use a form of meta-programming to analyze the program and
\emph{defunctionalize} it. We discuss this approach in section
\ref{sec:defunctionalization}.
\end{itemize}
