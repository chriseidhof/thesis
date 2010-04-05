%if False

> {-# LANGUAGE TypeOperators, GADTs, MultiParamTypeClasses,
>              FunctionalDependencies, TypeSynonymInstances 
>   #-}
>
> module Encoding where
> 
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Core (One, Many, Cardinality (..))

> newtype Email = Email { unEmail :: String }
> newtype URL   = URL   { unUrl   :: String }
> newtype Date  = Date  { unDate  :: String }

%endif

\label{sec:encoding}

From now on, we will use a simplified version of the ER model. We only consider
relationships between exactly entities. There are no attributes on
relationships.  Instead of letting the library user define a key to identify an
entity, we automatically add an integer attribute \attrib{id} for every entity
set. While these constrains might look tight, in practice they often work very
well. We have not yet looked at supporting full ER models, but we think our
approach forms a solid basis that can be extended.

To encode an ER model in Haskell, we start by
encoding the entity sets. We can encode an entity set $E_1$ as a Haskell type |T_1|. For
example, the entity set Haskell Compilers might be encoded as the datatype
|Compiler|. We call such a datatype an \emph{entity datatype}. Every entity
datatype has a single constructor with a field for every attribute. As an
example, we have encoded the entity sets from figure \ref{fig:ermodel}:


> data Compiler  = Compiler  {name :: String, homepage :: URL}
> data Person    = Person    {firstName :: String, lastName :: String, email :: Email}
> data Release   = Release   {title :: String, date :: Date, notes :: String}

An entity in the Haskell Compiler set is encoded as a Haskell value of type
|Compiler|. For example, the UHC compiler might be encoded as:

> uhc :: Compiler
> uhc = Compiler "UHC" (URL "http://www.cs.uu.nl/wiki/UHC")

Before we show how to encode relationship sets, consider the
\relationship{contributes} relationship set in Figure \ref{fig:contributes}.
Ultimately, when interfacing to a database, we might want to query the database
for the contributors of a compiler entity.  We then want to make sure that we get
a collection of contributors, not just one contributor. Also, we want to make sure that
every element in the collection is of the \entset{Person} entity datatype.
This can be achieved by storing both the entitity types and the cardinality of the
relationship set on the type level. For example, we could encode the
\relationship{contributes} relationship set by constructing a value of the
datatype |Rel|:

\begin{spec}
data Rel cardL entityL cardR entityR where
  Rel :: Rel cardL entitityL cardR entityR

contributes :: Rel Many Compiler Many Person
contributes = Rel

data One
data Many
\end{spec}

The |One| and |Many| types are datatypes without constructors, and are used as
type-level values. However, in order to program with them in Haskell, we need a
way to pattern-match on them. Also, we want to restrict the kinds of |cardL| and
|cardR| so that only values of |One| and |Many| can be provided. In order to do
this, we change |Rel| as following:

\begin{spec}
data Rel phi cardinalityL cardinalityR l r where
  Rel  ::  Cardinality cardinalityL 
       ->  Cardinality cardinalityR
       ->  Rel phi cardinalityL l cardinalityR r

data Cardinality a where
  One   :: Cardinality One
  Many  :: Cardinality Many
\end{spec}


At this point, we can define entities, entity sets and
relationship sets (note that \relationship{contributes} is a relationship set,
not a relation). In order to define an ER model, we have to combine the entity
sets and the relationship sets. Before we can do that, we change the type |Rel| to be
more restricted: we make sure that we 
only encode relationships between entities in the same ER model.

Therefore, we first define a type-level list, in a style similar to |HList|
\cite{kiselyov2004strongly},
that contains every entity type in the ER model.
We have experimented with other approaches such as multirec's |phi| datatype
\cite{rodriguez2009generic} to represent a fixed number of types.
However, multirec's approach does not allow for easy enumeration of the types,
whereas an |HList| does.

> type CompilerModel = Compiler :*: Person :*: Release :*: Nil

To refer to an element of the list, we can provide some indexes by hand:

> ixCompiler  ::  Ix CompilerModel Compiler
> ixCompiler  =   Zero
> ixPerson    ::  Ix CompilerModel Person
> ixPerson    =   Suc (Zero)
> ixRelease   ::  Ix CompilerModel Release
> ixRelease   =   Suc (Suc Zero)


We now
add an additional type-parameter to |Rel|, and two parameters to its
constructor, so that we can ensure the entities are in the same ER Model:

> data Rel model cardinalityL l cardinalityR r where
>   Rel  ::  Cardinality cardinalityL 
>        ->  Cardinality cardinalityR
>        ->  Ix model l
>        ->  Ix model r
>        ->  Rel phi cardinalityL l cardinalityR r

We could have changed the |Rel| constructor to have type-class constraints
instead of adding these fields, but having the indexes explicit will be handy
for pattern-matching, as we will see later on.

Our |contributes| relationship type has to change appropriately. We also define
the \relationship{releases} relationship.

> contributes ::  Rel  CompilerModel  Many  Compiler  Many  Person
> releases    ::  Rel  CompilerModel  One   Compiler  Many  Release
> contributes  = Rel  Many  Many  ixCompiler  ixPerson
> releases     = Rel  One   Many  ixCompiler  ixRelease

We enumerate all relationship sets on the type-level.

> type CompilerRelations  =    Rel CompilerModel  Many  Compiler  Many  Person
>                         :*:  Rel CompilerModel  One   Compiler  Many  Release
>                         :*:  Nil

We define a typeclass with a functional dependency that links the relationships to
the entity sets. For reasons that will become clear later on, we have used the
|TList4| type.

> class ERModel phi rels | phi -> rels, rels -> phi where
>   relations  :: TList4 Rel rels
> 
> instance ERModel CompilerModel CompilerRelations where
>   relations  =  TCons4 contributes
>              $  TCons4 releases
>              $  TNil4 

We now achieved the goal stated in in the introduction: the ER
model from the previous section is encoded in Haskell. In the next section,
we will build a simple in-memory database from the ground up. If you are
interested in the implementation, continue reading. If you are just interested
in using it, skip to section \ref{sec:inmeminterface} on page
\pageref{sec:inmeminterface} for a description of the interface.

Instead of giving the witnesses and |Rel| values explicitly, we might have
done everything implicitly and let the type-inferencer do the work. However, as
we will see in section \ref{sec:erconclusion}, the type errors quickly become
unmanageable.
