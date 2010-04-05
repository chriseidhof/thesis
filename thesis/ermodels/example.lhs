%if False

> {-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeSynonymInstances,
>     TypeFamilies, FlexibleContexts, RankNTypes #-}

> module Example where

> import Basil
> import qualified Data.Set as S
> import qualified Control.Monad.State as ST

> newtype Email = Email { unEmail :: String }
> newtype URL   = URL   { unUrl   :: String }
> newtype Date  = Date  { unDate  :: String }

%endif

This section describes how we can build an example in-memory database based on
the ER-model from section \ref{sec:encoding}.

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
an extra |String| parameter, which will be used in a later section, when we
build an interface to a relational database.

> type TContributes = Rel  CompilerModel  Many  Compiler  Many  Person
> type TReleases    = Rel  CompilerModel  One   Compiler  Many  Release
> contributes :: TContributes
> releases    :: TReleases
> contributes  = Rel  Many  ixCompiler  "compilers"  Many  ixPerson  "persons"
> releases     = Rel  One   ixCompiler  "compiler"   Many  ixRelease "releases"

Now we also need to enumerate the relationship sets on the type level:

> type CompilerRelations  =    TContributes
>                         :*:  TReleases
>                         :*:  Nil

For each relationship we create an index (again, this could be done using
Template Haskell):

> ixContributes = Zero
> ixReleases    = Suc Zero

We can give the instance for |ERModel| that links |CompilerModel| and
|CompilerRelations|:

> instance ERModel CompilerModel CompilerRelations where
>   relations  =  TCons4 contributes
>              $  TCons4 releases
>              $  TNil4 
>   witnesses  =  WCons ixCompiler $  WCons ixPerson $  WCons ixRelease $  WNil

Finally, we need some boilerplate code that is used to check if types are equal.
Again, we have not yet written the Template Haskell code for this.  

> type instance TypeEq Compiler     Compiler  = True 
> type instance TypeEq Compiler     Person    = False
> type instance TypeEq Compiler     Release   = False
> type instance TypeEq Person       Compiler  = False
> type instance TypeEq Person       Person    = True
> type instance TypeEq Person       Release   = False
> type instance TypeEq Release      Compiler  = False
> type instance TypeEq Release      Person    = False
> type instance TypeEq Release      Release   = True

Now we can construct a |Compiler| entity and two |Release| entities:

> ghc :: Compiler
> ghc = Compiler "GHC" (URL "http://haskell.org/ghc")

> ghc612 :: Release
> ghc612 = Release "6.12" (Date "11 Oct 2009") ""

> ghc610 :: Release
> ghc610 = Release "6.10" (Date "4 Nov 2008") ""

The |release| function constructs an |InitialValue| for a Compiler's release.
It is a 3-tuple that contains a reference to a |Compiler| entity, the direction
of the relationship and a pointer to the relationship.

> release :: Ref CompilerModel Compiler -> (Ref CompilerModel Compiler, Dir R, Ix CompilerRelations TReleases)
> release compiler = (compiler, DR, ixReleases)

> type M a = Basil CompilerModel CompilerRelations a

To create a new |Compiler| and a new |Release|, we can write the following code:

> example0 :: M (Ref CompilerModel Compiler)
> example0  = do  cId   <- new  ixCompiler  ghc     PNil
>                 rId   <- new  ixRelease   ghc612  (PCons (release cId) PNil)
>                 rId'  <- new  ixRelease   ghc610  (PCons (release cId) PNil)
>                 return cId

Note that the third argument is based on the |InitialValues| function from
section \ref{sec:initialvalues}. If we forget to include a |PList| with a
reference to the compiler, we get a type error. This way, the type system
ensures that we always provide the right initial relationships.

This creates a |Compiler| entity and adds two releases to it. We now write a
function that, given a reference to a |Compiler|, finds all releases:

> example1 :: Ref CompilerModel Compiler 
>          -> M (Maybe (S.Set (Ref CompilerModel Release)))
> example1 cId = findRels DL ixReleases cId

We can test both examples:

> runIt0 :: Ref CompilerModel Compiler
> runIt0  = fst (runBasil (example0))

Evaluating |runIt0| yields the following result:

\begin{spec}
Ref Fresh 0
\end{spec}

> runIt1 :: Maybe (S.Set (Ref CompilerModel Release))
> runIt1  = fst (runBasil (example0 >>= example1))

And evaluating |runIt2| yields the following result:

\begin{spec}
Just (fromList [Ref Fresh 1,Ref Fresh 2])
\end{spec}
