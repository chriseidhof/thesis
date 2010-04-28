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

In this section, we will encode an ER model in Haskell.
We capture the following constraints in the type system:

\begin{itemize}
\item Each entity in an entity set has the same attributes.
\item The cardinality of a relationship is one-to-one, one-to-many, many-to-one
or many-to-many.
\item Each relationship is between two entities in the same ER model
\end{itemize}

From here on, we use a simplified version of the ER model. We only consider
relationships with no attributes that relate exactly two entities.
We automatically add an integer attribute \attrib{id} as the primary key for
each entity.
In the section on future work we discuss these simplifications and how to extend
our approach to support full ER models.

We start with encoding entity sets.
An entity set $E_1$ is encoded as a Haskell datatype |T_1|. For
example, the entity set \emph{Haskell Compilers} is encoded as the datatype
|Compiler|. We call such a datatype an \emph{entity datatype}.
Each entity
datatype has exactly one constructor and a field for each attribute.
We have encoded the entity sets from figure \ref{fig:ermodel}:


> data Compiler  = Compiler  {name :: String, homepage :: URL}
> data Person    = Person    {firstName :: String, lastName :: String, email :: Email}
> data Release   = Release   {title :: String, date :: Date, notes :: String}

An entity in the Haskell Compiler set is encoded as a Haskell value of type
|Compiler|. For example, the UHC compiler might be encoded as:

> uhc :: Compiler
> uhc = Compiler "UHC" (URL "http://www.cs.uu.nl/wiki/UHC")

At this point, we have encoded entities and entity sets in Haskell. By using a
Haskell datatype to encode entity sets, we have encoded the first constraint:
each entity in an entity set has the same attributes.

To encode relationships we will give a first version of the datatype |Rel|,
which has four type parameters: one for each entity and two for the cardinality
of the relationship. As we will see later on, it is essential to encode the
entity types and cardinality on the type level if we want to get help from the
type checker to guarantee that the operations on entities and relationships are
correct.

\begin{spec}
data Rel cardL entityL cardR entityR where
  Rel :: Rel cardL entitityL cardR entityR
\end{spec}

To encode the cardinality, we introduce two datatypes which are solely used on
the type level, hence they have no constructors:

\begin{spec}
data One
data Many
\end{spec}

Using the |Rel|, |One| and |Many| datatypes, we can encode the |contributes| relationship:

\begin{spec}
contributes :: Rel Many Compiler Many Person
contributes = Rel
\end{spec}

However, the |contributes| relationship is not yet constrained in one of the two
ways stated in beginning of this section: it does not guarantee that both
entities are in the same ER model and it does not put any restriction on the
cardinality.

We start with restricting the cardinality, by introducing a datatype |Cardinality|,
which has a constructor for each cardinality. This way, we can construct a value
of |Cardinality One| and |Cardinality Many|, but no other values. 

\begin{spec}
data Cardinality a where
  One   :: Cardinality One
  Many  :: Cardinality Many
\end{spec}

We add two fields to our |Rel| constructor, one for each cardinality. This
restricts the cardinality: it is only possible to construct relationships that
have the cardinality one-to-one, one-to-many, many-to-one or many-to-many.

\begin{spec}
data Rel cardinalityL l cardinalityR r where
  Rel  ::  Cardinality cardinalityL 
       ->  Cardinality cardinalityR
       ->  Rel cardinalityL l cardinalityR r
\end{spec}

At this point, we can define entities, entity sets and
relationship sets (note that \relationship{contributes} is a relationship set,
not a relation). We have one more constraint that we want to guarantee:
relationships have to be between entities in the same ER model.
To add this constraint, we need a way to reason about all the entities in an ER
model.
As a solution, we define a type-level list, in a style similar to |HList|
\cite{kiselyov2004strongly} that contains an element for each entity type in the ER model.

> type CompilerModel = Compiler :*: Person :*: Release :*: Nil

The interface of the type-level list library is described in section
\ref{sec:erhlist}. It allows us to construct typed references into the
list, for example: these are the typed references for the |CompilerModel|. Each
reference is indexed on the type level by the list of entities and the entity it
points to:

> ixCompiler  ::  Ix CompilerModel Compiler
> ixCompiler  =   Zero
>
> ixPerson    ::  Ix CompilerModel Person
> ixPerson    =   Suc (Zero)
>
> ixRelease   ::  Ix CompilerModel Release
> ixRelease   =   Suc (Suc Zero)

We can now change |Rel| to enforce the final constraint. By storing typed
references in the |Rel| constructor that point into the same type-level list, we
encode that both entities are in the same ER model. We also add a type-parameter
|entities| to |Rel|, to encode that |Rel| is a relationship in that ER model.

> data Rel entities cardinalityL l cardinalityR r where
>   Rel  ::  Cardinality cardinalityL 
>        ->  Ix entities l
>        ->  Cardinality cardinalityR
>        ->  Ix entities r
>        ->  Rel entities cardinalityL l cardinalityR r

We could have changed the |Rel| constructor to have type-class constraints
instead of adding these fields, but having the references at the value level is handy
for pattern-matching, as we see later on.

Our \relationship{contributes} relationship type has to change accordingly. We also define
the \relationship{releases} relationship.

> contributes ::  Rel  CompilerModel  Many  Compiler  Many  Person
> contributes  = Rel  Many  Many  ixCompiler  ixPerson
>
> releases    ::  Rel  CompilerModel  One   Compiler  Many  Release
> releases     = Rel  One   Many  ixCompiler  ixRelease

To relate the entities and relationships to each other we add a type-class |ERModel|.
First, we enumerate all relationship sets in a
type-level list:

> type CompilerRelations  =    Rel CompilerModel  Many  Compiler  Many  Person
>                         :*:  Rel CompilerModel  One   Compiler  Many  Release
>                         :*:  Nil

Now we can define the type-class |ERModel| which is indexed by both the
type-level list of entities and the type-level list of relationships. The
entities and the relationships uniquely determine each other. We will explain
the |TList4| type in a later section.

> class ERModel entities rels | entities -> rels, rels -> entities where
>   relations  :: TList4 Rel rels
> 
> instance ERModel CompilerModel CompilerRelations where
>   relations  =  TCons4 contributes
>              $  TCons4 releases
>              $  TNil4 

We now have achieved the goal stated in in the introduction: the ER model from the
previous section is encoded in Haskell, and the constraints are guaranteed by
the typechecker.
