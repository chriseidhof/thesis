%if False

> {-# LANGUAGE GADTs #-}
> module Defunctionalization where
> import Monadic

%endif

Defunctionalization is a technique (TODO cite) to convert higher-order functional programs into first-order programs, i.e. programs without higher-order functions. We will explain the technique by defunctionalizing an example program.

Consider the following example, written in monadic style:

> example = do name <- form "Hello, what's your name?" id
>              display $ "Hi, " ++ name
>              num    <- form "Enter numbers:" sum'
>              display "OK"
>              return $ name ++ ", the sum is: " ++ show num

%if False

> sum' :: String -> Int
> sum' = sum . map read . words

%endif


If we rewrite this example without do-notation, we can see explicit lambdas:


> exampleCPS :: Web String
> exampleCPS =  
>  {- 1 -} form "Hello, what's your name?" id >>= 
>  {- 2 -}   \name -> display ("Hi, " ++ name) >>=
>  {- 3 -}      \() -> form "Enter numbers:" sum' >>= 
>  {- 4 -}        \num -> display "OK" >>=
>  {- 5 -}          \() -> return $ name ++ ", the sum is " ++ show num

We can now do manual defunctionalization. First, will create a data structure with a constructor for each function. That constructor contains a field for every free variable inside that function. For example, inside the last function both |name| and |l| are free. The datatype for our example program looks like this:


> data Defunc input where
>   D1  :: Defunc ()
>   D2  :: Defunc String
>   D3  :: String -> Defunc ()
>   D4  :: String -> Defunc Int
>   D5  :: String -> Int -> Defunc ()

Every constructor corresponds to a continuation and contains exactly the environment for executing it. We have parameterized the datatype over |input|. The type |input| corresponds to the type of the variable that is abstracted over. For example, |C4| corresponds to the lambda abstraction on lines 4-5, which takes an |Int| as parameter.

> applyC :: Defunc a -> (a -> Web String)
> applyC (D1)          ()    = form "Hello, what's your name?" id >>= applyC D2
> applyC (D2)          name  = display ("hi, " ++ name)           >>= applyC (D3 name)
> applyC (D3 name  )   ()    = form "Enter numbers"         sum'  >>= applyC (D4 name)
> applyC (D4 name  )   num   = display "ok!"                      >>= applyC (D5 name num)
> applyC (D5 name num) ()    = return $ name ++ ", the sum is " ++ show num

Finally, we can give the |exampleDefunc| function:

> exampleDefunc :: Web String
> exampleDefunc = applyC D1 ()

Defunctionalization is a whole-program transformation. It considers all functions in a program, creates a datastructure with a constructor for every function. The constructor has a field for every free variable of such a function.

While this is a concise description of defunctionalization, it is not that easy in Haskell. There are a couple of issues that arise. The first is observable sharing: how can we observe that a function is recursive, e.g.: how do we make sure that the datastructure we generate is finite? The second is calculating the free variables of a function: how can we inspect which variables are free? In the next subsections, we will look at two ways to deal with these problems: an embedded custom monad, and a preprocessor.

\subsection{A custom parameterized monad}

The key idea is that we will use the type system to perform analysis of our code. We will start out with a very simple Monad, which is only parameterized over its result type. Then we will add another parameter for the free variable analysis. Finally, we will see that we need two additional parameters that are used to give unique identifiers to every |bind| construct.

\begin{spec}
data Cont a where
  Use    :: IO a                     -> Cont a
  Bind   :: Cont a -> (a -> Cont b)  -> Cont b
\end{spec}

We can easily make |Cont| an instance of |Monad|. The |Cont| datatype makes the structure of the monadic expression explicit: the monadic binds are explicit in our |Cont| datatype. The first step we will take is extend the |Cont| datatype with an extra parameter |fV|, which stands for the free variables.

\begin{spec}
data Cont fv a where
  Use    :: IO a -> Cont fv a
  Bind   :: Cont fv1 a -> (a -> Cont fv2 b) -> Cont fv3 b
\end{spec}

However, the definition above is not done yet In the |Use| constructor, the |fv| in its type should correspond to the free variables used in its parameter. In the |Bind| constructor, the following rule applies:

$ fv_3 = fv_1 \cup (fv_2 - \{a\}) $

An improvement would be to use a |Ref| datatype that refers to a free variable. The exact implementation is not important now, and it will have only one type parameter, the type of the value it refers to:

\begin{spec}
 data Ref a
\end{spec}

\begin{spec}
data Cont fv a where
  Use  :: (Ref free -> IO b) -> Cont free b
  Bind :: Cont fv1 a -> (Ref a -> Cont fv2 b) -> Cont fv3 b
\end{spec}

We can now see which variables are used. However, the type variable |fv3| in the |Bind| constructor is still not correct: it needs to be the union of the type variables |fv1| and |fv2|, where |a| is removed from |fv2|.

First, we will change the |Use| constructor to return a list of references to free variables (even though, in our case, there is only one free variable). The |Singleton| type is a type synonym for a singleton heterogenerous list.

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

Instead of explaining how this works, we will ask the following question: is this acceptable? Our goal was to hide complexity in the library, providing a clean and simple interface for the library user. We have now arrived at such a complex datatype that it is almost impossible for most people to use it. When the library user makes a mistake, the type errors will become really complex.  Therefore, we have refrained from further developing this datatype, because Haskell's type system is not meant for or useable for these kinds of analyses.

\subsection{A preprocessor}

We have also looked at writing a preprocessor for our language. Using a preprocessor, free variable analysis is easy and we can use a straightforward textbook implementation. However, when generating the datatype used for defunctionalization, we need the type of every variable. This is not easily accessible from the source language, unless we explicitly type every variable that is bound. This also not a way forward, as it puts a heavy burden on the user of the language.

\subsection{The ideal situation}

Defunctionalization can be done on an abstract syntax tree that is annotated with types. Currently, this is not possible, due to the limitations of the Haskell language. We can envision a language where it is possible to write program transformations on fully typed abstract syntax tree, that would allow us to write down the defunctionalization in a concise way.

For our purposes, unfortunately, automatic defunctionalization of Haskell programs is too hard.
