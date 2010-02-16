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
each other. Notice that there are no functions with hidden state. The |Thread|
constructor threads the result of the first value through.

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

We can run a single page and get a result:

> runPage :: Page i o -> i -> Result o
> runPage (Fun f)        = Done . f
> runPage (Form s f)     = const (Step s   () (Req >>> arr f))
> runPage (Display msg)  = \i -> Step (msg i) () (arr (const ()))

There are two combinators for sequencing, the |>>>| combinator just sequences
two interactions, and the |>&>| combinator also remembers the result from the
first interaction.

> (>>>) :: Web a b -> Web b c -> Web a c
> (>>>) = Seq

> infixl >&>
> (>&>) :: Web a b -> Web b c -> Web a (b,c)
> (>&>) = Thread

The |arr| combinator lifts a pure function into the |Web| type:

> arr :: (a -> b) -> Web a b
> arr = Single . Fun

Handling a request is simple:

> handleRequest :: Web i o -> i -> Request -> Result o
> handleRequest (Single page)  inp req = runPage page inp
> handleRequest (Req)          inp req = Done req
> handleRequest (Seq    l r)   inp req = case handleRequest l inp req of
>                                          Done res          -> handleRequest r res req
>                                          Step page i cont  -> Step page i (cont >>> r)
> handleRequest (Thread l r)   inp req = case handleRequest l inp req of
>                                          Done res -> handleRequest (r >>> arr ((,) res)) res req
>                                          Step page i cont -> Step page i (Thread cont r)

Before we define an example, we first provide some smart constructors:

> fun :: (i -> o) -> Web i o
> fun      = Single . Fun
>
> form :: String -> (String -> o) -> Web i o
> form x   = Single . Form x
>
> display :: (i -> String) -> Web i ()
> display  = Single . Display

Now we can define our example. After the first form the |>&>| combinator is
used, to pass sure the value to the final |display| page.

> example  =    form "Hi, what's your name?" id
>          >&>  form "Enter two numbers:" (map (read :: String -> Int) . words)
>          >>>  display finalPage
>  where  sum        = (+) :: Int -> Int -> Int
>         finalPage name [x,y] = "Hi " ++ name ++ ", the sum is: " ++ show (sum x y)

We can build an interactive evaluator in the console that reads out requests and
runs a |Web| computation:

> runConsole :: a -> Web a () -> IO ()
> runConsole a w = do putStr "Request> "
>                     req <- getLine
>                     case handleRequest w a req of
>                       Done ()       -> putStrLn "Done"
>                       Step msg i w' -> do putStrLn msg
>                                           runConsole i w'

\subsection{Tracing}

Everytime we come across a bind, we can emit a trace step. From a full list of trace steps we can recover a program point.

\subsection{Observable sharing}

We now proceed to the serialization of a |Web| value. In order to do that,
we will first convert a recursive |Web| value into an explicit graph with
observable sharing. This is done using the \library{data-reify} library (TODO cite) 
from Gill (TODO cite). To see how this works, we can
take a look at a simple example for lists. Consider the following two mutually
recursive expressions:

\begin{spec}
data List a = Nil | Cons a (List a)
x = Cons 'a' y
y = Cons 'b' x
\end{spec}

If we try to inspect the structure of this expression, we get an infinite value.
In order to work with \library{data-reify}, we first change our data-structure
to its \emph{pattern-functor} (TODO cite multirec):

\begin{spec}
newtype Fix a     = In {out :: a (Fix a)}
data PF_List a r  = NilF | ConsF a r deriving Show
type List' a      = Fix (PF_List a)
\end{spec}

Now we can redefine |x| and |y| using our new datatype:

\begin{spec}
x' = In (ConsF 'a' y')
y' = In (ConsF 'b' x')
\end{spec}

If we make the type |PF_List| an instance of |Traversable|, we can reify the
recursive value |x'|, yielding the following result:

\begin{spec}
let [(1,ConsF 'a' 2),(2,ConsF 'b' 1)] in 1
\end{spec}

The result is a \emph{finite} list of key/value pairs, where each key represents
a node in the graph. The recursive positions have been replaced by a reference
to such a key.  Because our program is always finite, it should always be
possible to build such a graph.


