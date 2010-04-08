%if False

> {-# LANGUAGE GADTs, ExistentialQuantification, TypeSynonymInstances,
> FlexibleContexts, PackageImports #-}
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

> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Protocol.Uri.Data
> import Network.Salvia.Handlers
> import Network.Salvia.Impl.Config
> import Network.Salvia.Impl.Server
> import Network.Salvia.Interface
> import Data.Record.Label
> import Control.Category
> import Prelude hiding ((.))
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
type-paramater indicates the result of running a |Web| computation. Note that
there are no functions in the new |Web| type. Composition is instead expressed
using the |Seq| constructor. If we compose to |Web| values that produce a |b|
and a |c|, respectively, and we want to reuse the |b| value in a later
computation, we can use the |Thread| constructor, which threads a value to the
next computation.

> data Web i o where
>   Single  :: Page i o -> Web i o
>   Req     :: Web () RequestBody
>   Seq     :: Web a b -> Web b c -> Web a c
>   Thread  :: Web a b -> Web b c -> Web a (b,c)
>   Choice  :: Web a Bool -> Web a b -> Web a b -> Web a b

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

Now we can define the |Result| datatype, which is very similar to the |Result|
datatype from the previous section. However, our |Continuation| datatype does
not use functions to represent the environment.

> data Result o where
>   Done    :: o -> Result o
>   Step    :: X.Html -> Continuation o -> Result o

To get the result of a single page, we provide the |runPage| function:

> runPage :: Page i o -> i -> Result o
> runPage (Fun f)           i  = Done (f i)
> runPage (Form msg parse)  _  = Step msg     (Cont () (runForm msg parse))
> runPage (Display msg)     i  = Step (msg i) (Cont () (fun (const ())))

To run a |Form|, we parse the request, and if it is a |Failure|, we restart the
flow. Instead of rendering the form as usual, we also add the error messages.
If the form is entered correctly, we can return the value.
The function |fromSuccess| converts a |Failing a| into an |a|.

> runForm :: X.Html -> (RequestBody -> Failing b) -> Web () b
> runForm formHtml parse  = start
>  where start =      Req 
>              `Seq`  fun parse
>              `Seq`  Choice (fun isFailure)
>                     retry 
>                     (fun fromSuccess)
>        retry = display showFormWithError `Seq` start
>        showFormWithError (Failure msgs) = X.toHtml msgs X.+++ formHtml

%if False

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

> handleRequest :: Web i o -> i -> RequestBody -> Result o
> handleRequest (Single page) inp body = runPage page inp
> handleRequest (Req)         inp body = Done body
> handleRequest (Seq    l r)  inp body = case handleRequest l inp body of
>   Done res              -> handleRequest r res body
>   Step page (Cont i c)  -> Step page (Cont i (c `Seq` r))
> handleRequest (Thread l r)  inp body = case handleRequest l inp body of
>   Done res              -> handleRequest (r `Seq` fun ((,) res)) res body
>   Step page (Cont i c)  -> Step page (Cont i (Thread c r))


> type Env = M.Map String (Continuation ())

> run :: Env -> String -> RequestBody -> (X.Html, Env) 
> run env page reqBody = case M.lookup page env of
>                        Nothing   -> (pageNotFound, env)
>                        Just (Cont i c) ->
>                          let np = "/" ++ page
>                              result        = handleRequest c i reqBody -- np?
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
>         -- This echoes the (parsed) query paramaters.
>         (hDefaultEnv (do e' <- liftIO $ takeMVar env
>                          h <- hCurrentPath
>                          liftIO $ putStrLn $ show h
>                          contId <- (intercalate "/". __segments) <$> hCurrentPath
>                          liftIO $ putStrLn contId
>                          let formInputs = undefined
>                          let (html, e') = run e contId formInputs
>                          liftIO $ putMVar env e'
>                          response (contentType =: Just ("text/html", Just "utf-8"))
>                          send (show html)
>                      ))
>         ()

> hCurrentPath :: HttpM Request m => m Path
> hCurrentPath = request (getM (_path . asUri))


>
> -- createServerPart :: Env -> IO (ServerPart Response)
> -- createServerPart e = do env <- newMVar e
> --                         return $ ServerPartT $ handle env
> -- 
> -- handle :: MVar Env -> ReaderT Request (WebT IO) Response
> -- handle env = do req <- ask
> --                     formInputs = map (\(k,v) -> (k, B.unpack $ inputValue v)) $ rqInputs req
> --                 e <- liftIO $ takeMVar env
> --                 let (html, e') = run e contId formInputs
> --                 liftIO $ putMVar env e'
> --                 return $ toResponse html

> exampleEnv = M.singleton "/arc" (Cont () arc)

> main = runServer 8080 exampleEnv

> 
> arc :: Web () ()
> arc =        input 
>     `Thread` (link "Click Here")
>     `Seq`    (display (\(name, ()) -> X.toHtml $ "Hello, " ++ name))

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
 
% There are two combinators for sequencing, the |>>>| combinator just sequences
% two interactions, and the |>&>| combinator also remembers the result from the
% first interaction.
% 
% > (>>>) :: Web a b -> Web b c -> Web a c
% > (>>>) = Seq
% 
% > infixl >&>
% > (>&>) :: Web a b -> Web b c -> Web a (b,c)
% > (>&>) = Thread
% 
% The |arr| combinator lifts a pure function into the |Web| type:
% 
% > arr :: (a -> b) -> Web a b
% > arr = Single . Fun
% 
% Before we define an example, we first provide some smart constructors:
% 
% > fun :: (i -> o) -> Web i o
% > fun      = Single . Fun
% >
% > form :: String -> (String -> o) -> Web i o
% > form x   = Single . Form x
% >
% > display :: (i -> String) -> Web i ()
% > display  = Single . Display
% 
% Now we can define our example. After the first form the |>&>| combinator is
% used, to pass sure the value to the final |display| page.
% 
% > example  =    form "Hi, what's your name?" id
% >          >&>  form "Enter two numbers:" (map (read :: String -> Int) . words)
% >          >>>  display finalPage
% >  where  sum        = (+) :: Int -> Int -> Int
% >         finalPage name [x,y] = "Hi " ++ name ++ ", the sum is: " ++ show (sum x y)
% 
% We can build an interactive evaluator in the console that reads out requests and
% runs a |Web| computation:
% 
% > runConsole :: a -> Web a () -> IO ()
% > runConsole a w = do putStr "Request> "
% >                     req <- getLine
% >                     case handleRequest w a req of
% >                       Done ()       -> putStrLn "Done"
% >                       Step msg i w' -> do putStrLn msg
% >                                           runConsole i w'
% 
% \subsection{Tracing}
% 
% Everytime we come across a bind, we can emit a trace step. From a full list of trace steps we can recover a program point. However, the trace steps will keep growing for every step the user takes. If we consider recursive programs, the trace steps may grow infinitely. This is the approach that WASH takes. In the next section, we will look at a way to prevent these infinite traces.
% 
% \subsection{Observable sharing}
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
