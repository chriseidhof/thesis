%if False

> {-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeSynonymInstances,
>     TypeFamilies, FlexibleContexts, RankNTypes #-}

> module Example where

> import Basil
> import Basil.InMemory
> import qualified Data.Set as S

> newtype Email = Email { unEmail :: String }
> newtype URL   = URL   { unUrl   :: String }
> newtype Date  = Date  { unDate  :: String }

%endif

This section describes how we can build an example in-memory database based on
the ER-model from section \ref{sec:encoding}.
The library used in this example is described in the previous sections and summarized in sections \ref{sec:ercoreif} and
\ref{sec:inmemif}.

First, recall the entity datatypes:

> data Compiler  = Compiler  {name :: String, homepage :: URL}
> data Person    = Person    {firstName :: String, lastName :: String, email :: Email}
> data Release   = Release   {title :: String, date :: Date, notes :: String}

Next, we need to define a type-level list with all the entity types:

> type CompilerModel = Compiler :*: Person :*: Release :*: Nil

To refer to an element of the list, we can again provide the indexes by hand.
Ultimately, this could be done using a Template Haskell function.

> ixCompiler  ::  Ix CompilerModel Compiler
> ixCompiler  =   Zero
> ixPerson    ::  Ix CompilerModel Person
> ixPerson    =   Suc (Zero)
> ixRelease   ::  Ix CompilerModel Release
> ixRelease   =   Suc (Suc Zero)

We define the |contributes| and |releases| relationship sets. Note that we give
an extra |String| parameter, which is used in a later section, when we
build an interface to an external relational database.

> type TContributes  = Rel  CompilerModel  Many  Compiler  Many  Person
> type TReleases     = Rel  CompilerModel  One   Compiler  Many  Release
> contributes :: TContributes
> releases    :: TReleases
> contributes  = Rel  Many  ixCompiler  "compilers"  Many  ixPerson  "persons"
> releases     = Rel  One   ixCompiler  "compiler"   Many  ixRelease "releases"

The next step is to construct a type-level list with all the relationships:

> type CompilerRelations  =    TContributes :*:  TReleases :*:  Nil

For each relationship we create an index:

> ixContributes  ::  Ix CompilerRelations TContributes
> ixContributes  =   Zero
>
> ixReleases  ::  Ix CompilerRelations TReleases
> ixReleases  =   Suc Zero

We give the instance for |ERModel| that links |CompilerModel| and
|CompilerRelations|:

> instance ERModel CompilerModel CompilerRelations where
>   relations  =  TCons4 contributes
>              $  TCons4 releases
>              $  TNil4 
>   witnesses  =  WCons ixCompiler $  WCons ixPerson $  WCons ixRelease $  WNil

Finally, we need some boilerplate code that is used to check if two types are equal.
We have written Template Haskell that generates this, but it is instructive to do it by hand.

> type instance TypeEq Compiler     Compiler  = True 
> type instance TypeEq Compiler     Person    = False
> type instance TypeEq Compiler     Release   = False
> type instance TypeEq Person       Compiler  = False
> type instance TypeEq Person       Person    = True
> type instance TypeEq Person       Release   = False
> type instance TypeEq Release      Compiler  = False
> type instance TypeEq Release      Person    = False
> type instance TypeEq Release      Release   = True


The |release| function constructs an |InitialValue| for a Compiler's release.
It is a tuple that contains a reference to a |Compiler| entity, the direction
of the relationship and a pointer to the relationship.

> release  ::  Ref CompilerModel Compiler 
>          ->  (Ref CompilerModel Compiler, Dir R, Ix CompilerRelations TReleases)
> release compiler = (compiler, DR, ixReleases)

> type M a = Basil CompilerModel CompilerRelations a

To create a new |Compiler| and add two new |Release|s to it, we write the following code:

> example0 :: M (Ref CompilerModel Compiler)
> example0  = do  cId   <- new  ixCompiler  ghc     PNil
>                 rId1  <- new  ixRelease   ghc612  (PCons (release cId) PNil)
>                 rid2  <- new  ixRelease   ghc610  (PCons (release cId) PNil)
>                 return cId

Note that the third argument is based on the |InitialValues| function from
section \ref{sec:initialvalues}. If we forget to include a |PList| with a
reference to the compiler, we get a type error. This way, the type system
ensures that we always provide the right initial relationships.

The value |ghc| is a |Compiler| entity, the values |ghc612| and |ghc610| are |Release| entities:

> ghc :: Compiler
> ghc = Compiler "GHC" (URL "http://haskell.org/ghc")
>
> ghc612 :: Release
> ghc612 = Release "6.12" (Date "11 Oct 2009") ""
>
> ghc610 :: Release
> ghc610 = Release "6.10" (Date "4 Nov 2008") ""

To find all releases that belong to a |Compiler| entity, we write the following code:

> example1  ::  Ref CompilerModel Compiler 
>           ->  M (Maybe (S.Set (Ref CompilerModel Release)))
> example1 cId = findRels DL ixReleases cId

To test the first example, we can use the |runBasil| function:

> runIt0 :: Ref CompilerModel Compiler
> runIt0  = fst (runBasil example0)

Evaluating |runIt0| yields the following result:

\begin{spec}
Ref 0
\end{spec}

The second example combines both |example0| and |example1| into a new example:

> runIt1 :: Maybe (S.Set (Ref CompilerModel Release))
> runIt1  = fst (runBasil (example0 >>= example1))

And evaluating |runIt2| yields the following result:

\begin{spec}
Just (fromList [Ref 1,Ref 2])
\end{spec}

We have now written an in-memory database that derives its schema from the ER model described in section \ref{sec:encoding}.
The in-memory database guarantees that all relationships are correctly initialized when creating a new entity, and by encoding entities as Haskell record datatypes we guarentee that all attributes of an entity are provided.
