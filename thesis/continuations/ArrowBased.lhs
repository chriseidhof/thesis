%if False

> {-# LANGUAGE GADTs #-}
> module Example where

%endif

As we have seen in the previous section, web continuations can be expressed as
monads. The main idea of this section is to represent a web continuation as a
directed graph.  We do allow cycles, but because our program is finite, our
graph is finite too: we have a finite number of nodes and a finite number of
edges.

We will build an arrow-like interface.  We reuse the |Request| type from the
previous section:

> type Request = String

In the previous section our |Web| type was only parameterized by the output, and
could be made an instance of |Monad|. Arrows are not only parameterized by their
output, but also by their input. The first thing we do is extend the |Page| type
with an extra type parameter for the input and an extra constructors that lifts
functions into a value of |Page|:

> data Page i o where
>   Fun      :: (i -> o) -> Page i o
>   Form     :: String -> (String -> o) -> Page a o
>   Display  :: (a -> String) -> Page a ()

Web interactions can then be defined as single pages or two pages chained to
each other. Notice that there are no functions with hidden state:

> data Web i o where
>   Single  :: Page i o -> Web i o
>   Req     :: Web () Request
>   Seq     :: Web a b -> Web b c -> Web a c
>   Thread  :: Web a b -> Web b c -> Web a (b,c)

As a result of running a |Web| continuation, we get either a simple value or a
page with a continuation.

> data Result o where
>   Done  :: o -> Result o
>   Step  :: String -> i -> Web i o -> Result o

We can run a single page and 

> runPage :: Page i o -> i -> Result o
> runPage (Fun f)        = Done . f
> runPage (Form s f)     = const (Step s   () (Req >>> arr f))
> runPage (Display msg)  = \i -> Step (msg i) () (arr (const ()))

> (>>>) :: Web a b -> Web b c -> Web a c
> (>>>) = Seq

> infixl >&>
> (>&>) :: Web a b -> Web b c -> Web a (b,c)
> (>&>) = Thread

> arr :: (a -> b) -> Web a b
> arr = Single . Fun

> handleRequest :: Web i o -> i -> Request -> Result o
> handleRequest (Single page) inp req = runPage page inp
> handleRequest (Req)         inp req = Done req
> handleRequest (Seq    l r)  inp req = case handleRequest l inp req of
>                                         Done res         -> handleRequest r res req
>                                         Step page i cont -> Step page i (cont >>> r)
> handleRequest (Thread l r)  inp req = case handleRequest l inp req of
>                                         Done res -> handleRequest (r >>> arr ((,) res)) res req
>                                         Step page i cont -> Step page i (Thread cont r)

> fun :: (i -> o) -> Web i o
> fun      = Single . Fun

> form :: String -> (String -> o) -> Web i o
> form x   = Single . Form x

> display :: (i -> String) -> Web i ()
> display  = Single . Display


> example  =    form "Hi, what's your name?" id
>          >&>  form "Enter two numbers:" (map (read :: String -> Int) . words)
>          >>>  display (\(name, [x,y]) -> "Hi " ++ name ++ ", the sum is: " ++ show (sum x y))
>  where sum = (+) :: Int -> Int -> Int

> runConsole :: a -> Web a () -> IO ()
> runConsole a w = do putStr "Request> "
>                     req <- getLine
>                     case handleRequest w a req of
>                       Done ()       -> putStrLn "Done"
>                       Step msg i w' -> do putStrLn msg
>                                           runConsole i w'
