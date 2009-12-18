%if False

> {-# LANGUAGE GADTs #-}
> module Example where
> import qualified Data.Map as M

%endif

In this section we will build a sketch of a Monadic continuation-based
programming library. We have simplified things for the sake of the presentation.
We assume requests are simple |String| values:

> type Request = String

In our module, a page is either a form of |a| or a basic webpage displaying
something. The first parameters for |Form| is the rendering of the form, the
second parameter is the parsing.

> data Page a where
>   Form     :: String -> (String -> a) -> Page a
>   Display  :: String -> Page ()

A web continuation is just a function from |Request| to |Result|:

> newtype Web a = Web {runWeb :: Request -> Result a}

When running a web continuation, either the computation is completely finished,
or it displays a page with a continuation.

> data Result a  =  Done a
>                |  Step String (Web a)

From a single page we can calculate a function that ignores the request and
produces a |Result|:

> runPage :: Page a -> Request -> Result a
> runPage (Form msg parse)  _  = Step msg (Web (Done . parse))
> runPage (Display msg)     _  = Step msg (return ())

We can derive two smart constructors that lift a |Page| directly into the |Web|
type:

> display  :: String -> Web ()
> display  = Web. runPage . Display 
> form     :: String -> (String -> a) -> Web a
> form m   = Web . runPage . Form m

The |Web| newtype can easily be made an instance of |Monad|:

> instance Monad Web where
>   return   = Web . const . Done
>   l >>= r  = Web $ \req -> case (runWeb l req) of
>                Done x       -> runWeb (r x) req
>                Step msg l'  -> Step msg (l' >>= r) 

Now we are ready to write command-line evaluator for |Web|.

> handleRequest :: Web () -> IO ()
> handleRequest (Web w) = do  putStr "Request> "
>                             ln <- getLine
>                             handleResult (w ln)
> handleResult :: Result () -> IO ()
> handleResult (Done a)         = do  putStrLn $ "Done."
> handleResult (Step msg cont)  = do  putStrLn $ "Response: " ++ msg
>                                     handleRequest cont

We can build an example interactive program:

> example = do  name     <-  form "Hello, what's your name?" id
>               [x,y]    <-  form "Enter two numbers:" (map read . words)
>               display  $   name ++ ", the sum is: " ++ show (sum x y)
>                where sum = (+) :: Int -> Int -> Int

Here's an example run in GHCi:

\begin{verbatim}
Request> 
Response: Hello, what's your name?
Request> chris
Message: Enter two numbers seperated by a space:
Request> 10 20
Message: chris, the sum is: 30
Request> 
Done.
\end{verbatim}

From here on, we could extend the library to work with a webserver, serve HTML
instead of simple strings, and so on. However, although our continuation type
is very simple, a function with type |Request -> Result ()|, it still is a
function type. Even if we wrap it in other types, there will be a function type
somewhere. In Haskell, it is impossible to serialize function types. Therefore,
we will look at an alternative approach in the next section.
