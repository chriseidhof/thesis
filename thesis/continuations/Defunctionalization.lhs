%if False

> {-# LANGUAGE GADTs #-}
> module Defunctionalization where
> import Monadic

%endif

Defunctionalization is a technique to convert higher-order functional programs
into first-order programs, i.e. programs without higher-order functions
\cite{reynoldsdefunctionalization, danvy2001defunctionalization}.
We explain the technique by defunctionalizing an example program.
After that, we try to embed defunctionalization in the Haskell type system
by using an index monad. However, the code is not finished.

Consider the following example, written in monadic style:

> example = do  name <- form "Hello, what's your name?" id
>               display $ "Hi, " ++ name
>               num    <- form "Enter numbers:" sum'
>               display "OK"
>               return $ name ++ ", the sum is: " ++ show num

%if False

> sum' :: String -> Int
> sum' = sum . map read . words

%endif


If we rewrite this example without do-notation, we can see the lambdas. We have
numbered each lambda:


> exampleCPS :: Web String
> exampleCPS =  
>  {- 1 -} form "Hello, what's your name?" id >>= 
>  {- 2 -} \name  -> display ("Hi, " ++ name) >>=
>  {- 3 -} \()    -> form "Enter numbers:" sum' >>= 
>  {- 4 -} \num   -> display "OK" >>=
>  {- 5 -} \()    -> return $ name ++ ", the sum is " ++ show num

We can now do manual defunctionalization.
First, we create a data structure with a constructor for each function.
That constructor contains a field for every free variable inside that function.
For example, inside the last function both |name| and |l| are free variables.
The datatype for our example program looks like this:

> data Defunc input where
>   D1  :: Defunc ()
>   D2  :: Defunc String
>   D3  :: String -> Defunc ()
>   D4  :: String -> Defunc Int
>   D5  :: String -> Int -> Defunc ()

Every constructor corresponds to a continuation and contains exactly the
environment for executing it.
We have parameterized the datatype over |input|, which is the type of the
variable that is bound by the corresponding lambda.
The type |input| corresponds to the type of the variable that is abstracted over.
For example, |C4| corresponds to the lambda abstraction on lines 4-5, which
takes an |Int| as parameter.

To interpret a |Defunc| value, we can define an |apply| function that takes a
|Defunc a| and the input of type |a|, and produces a |Web| computation:

> applyC :: Defunc a -> a -> Web String
> applyC (D1)          ()    = form "Hello, what's your name?" id >>= applyC D2
> applyC (D2)          name  = display ("hi, " ++ name)           >>= applyC (D3 name)
> applyC (D3 name  )   ()    = form "Enter numbers"         sum'  >>= applyC (D4 name)
> applyC (D4 name  )   num   = display "ok!"                      >>= applyC (D5 name num)
> applyC (D5 name num) ()    = return $ name ++ ", the sum is " ++ show num

Finally, we can give the |exampleDefunc| function, which is the defunctionalized
variant of |exampleCPS|:

> exampleDefunc :: Web String
> exampleDefunc = applyC D1 ()

A defunctionalized program is explicit about its environment.
For example, the |D5| constructor stores in its environment a |String| and an
|Int| value.
Because a |Defunc| value encodes a program pointer as well as its environment,
it can be seen as an equivalent of a continuation. 
However, it is straightforward to serialize the |Defunc| datatype, as opposed to
serializing normal Haskell functions.

Defunctionalization is a program transformation that works as a whole program
transformation.
It needs to inspect bindings, sharing and the types of variables.
In Haskell, we cannot inspect bindings and sharing of a function. In Template
Haskell, however, we can inspect the entire program, but we do not have type
information available.

To provide a workaround for these problems, we use the next section to introduce
a monad that is explicit about the environment and types in the environment. 
It does not inspect sharing, but it is a first step in the direction of
automatic defunctionalization.

\subsection{A custom parameterized monad}

The key idea is to use the type system for analysis of our
code, by building an indexed monad.
The idea to encode a program analysis in the type system has been around for some time in the Haskell community
\cite{russo2008library, pucella2009haskell, fluet2006monadic}.

We start out with a very simple Monad, which is only parameterized over its result type.
Then we add another parameter for the free variable analysis.
Finally, we see that we need two additional parameters that are used to give unique identifiers to every |bind| construct.

\begin{spec}
data Cont a where
  Use    :: IO a                     -> Cont a
  Bind   :: Cont a -> (a -> Cont b)  -> Cont b
\end{spec}

We can easily make |Cont| an instance of |Monad|. The |Cont| datatype makes the structure of the monadic expression explicit: the monadic binds are explicit in our |Cont| datatype. The first step we take is extend the |Cont| datatype with an extra parameter |fV|, which stands for the free variables.

\begin{spec}
data Cont fv a where
  Use    :: IO a -> Cont fv a
  Bind   :: Cont fv1 a -> (a -> Cont fv2 b) -> Cont fv3 b
\end{spec}

However, the definition above is not done yet In the |Use| constructor, the |fv| in its type should correspond to the free variables used in its parameter. In the |Bind| constructor, the following rule applies:

$ fv_3 = fv_1 \cup (fv_2 - \{a\}) $

An improvement would be to use a |Ref| datatype that refers to a free variable.
The exact implementation is not important now. It has only one type parameter, the type of the value it refers to:

\begin{spec}
 data Ref a
\end{spec}

\begin{spec}
data Cont fv a where
  Use  :: (Ref free -> IO b) -> Cont free b
  Bind :: Cont fv1 a -> (Ref a -> Cont fv2 b) -> Cont fv3 b
\end{spec}

We can now see which variables are used. However, the type variable |fv3| in the |Bind| constructor is still not correct: it needs to be the union of the type variables |fv1| and |fv2|, where |a| is removed from |fv2|.

First, we change the |Use| constructor to return a list of references to free variables (even though, in our case, there is only one free variable). The |Singleton| type is a type synonym for a singleton heterogenerous list.

\begin{spec}
 data Cont fv a where
   Use  :: (Ref free -> IO b) -> Cont free (Singleton (Ref free ))
   Bind :: Cont fv1 a -> (Ref a -> Cont fv2 b) -> Cont fv3 b
\end{spec}

Lists are something we can deal with on the type-level. For example, we can now write the following definition for the |Bind| constructor, using type-level append function |App|. The constraint |App fv1 fv2 fv3|.

\begin{spec}
 Bind   :: ( App fv1 fv2 fv3
           )
        => Cont fv1 a 
        -> (Ref a -> Cont fv2 b)
        -> Cont fv3 b
\end{spec}

However, we are not done yet. We also want to remove the |Ref a| from |fv2| to really obtain the free variables of that function. And while we are at it, we also want to remove duplicates in the list that come from appending |fv1| and |fv2|. Removing duplicates is not that easy. We can not simply compare two |Ref| types. Therefore, we add a type parameter to |Ref| that is the index of the |Bind| constructor in the program. For completeness, we present the following type, that adds two type-level numbers as parameters:

\begin{spec}
data Cont freeVars from to a where
  Simple :: IO a -> Cont Nil from from a
  Use    :: Ref freeVars n 
         -> (freeVars -> IO a)
         -> Cont (Sing (Ref freeVars n)) from from a
  Bind   :: ( App freeVars1 freeVars2 zs
            , Remove to1 zs cleaned
            , FilterDoubles cleaned uniq
            )
         => Cont freeVars1 from to1 a 
         -> (Ref a (Succ to1) -> Cont freeVars2 (Succ to1) to2 b)
         -> Cont uniq from to2 b
\end{spec}

We have not continued work on this approach beyond this point.
The original reason to use monads instead of arrows is because it is easier for the user.
However, using this library is quite hard, because we can not hide the
complexity of the type-level functions from the end user.
If she makes a simple mistake, she can get a complicated type error that obfuscates
the real problem.
