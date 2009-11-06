\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
%format :-> = "\mapsto"
%format +++ = "+\!\!\!+\!\!\!+"
%format << = "<\!\!<"
%format >>> = ">\!\!>\!\!>"
%format phi = "\phi"
%format T_1
% vim:spell
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}
\usepackage{hyperref}

\newcommand{\entset}[1]{\emph{#1}}
\newcommand{\attrib}[1]{\emph{#1}}
\newcommand{\relationship}[1]{\emph{#1}}

\begin{document}
\author{Chris Eidhof}
\title{Describing ER models in Haskell}

\maketitle


\section{Introduction}

Designing a database model is often done by creating an entity-relationship (ER)
model (TODO: cite chen). An ER model is a declarative specification of a database layout: it
describes the entities involved and their relationships. An ER model can be used
to derive a database schema. In this paper, we will see how to encode ER
models in Haskell, how to derive an in-memory database for such a model and how
to derive a schema and interface for a relational database.

% TODO: move to section on the end?
Compared to HaskellDB, the approach we present here is more high-level.
HaskellDB is a type-safe way to deal with relational databases, whereas our
library is not tailored to a specific storage engine such as a relational
database, but is a more general way of describing data models.

In the next section we will introduce what an ER model is. After introducing the
necessary vocabulary and concepts we will show how to encode this in Haskell.
Next, we will build an in-memory database in Haskell and interface to a
relational database.

\section{ER models}

An entity is an object in the real world. Examples of an entity are: the UHC Haskell
compiler, the Haskell website or the manager of the Haskell website. Similar
entities are grouped into entity sets. Example entity sets include: the
collection of Haskell compilers, the collection of websites and the collection
of people working on Haskell.

An entity is described using attributes, which map an entity to values. Attributes of a
Haskell compiler might be \attrib{name} and \attrib{homepage}. All
entities in an entity set have the same attributes. Every attribute has a
domain, for example, the domain of a compiler's name might be the set of all
strings, the domain of release date might be all dates.  For each entity set,
there is a key, which uniquely identifies an entity. In Figure
\ref{fig:compilers}, we see an example of the Haskell Compiler entity set.

\begin{figure}
\includegraphics[width=5cm]{ermodels/compiler}
\caption{The Compiler entity set}
\label{fig:compilers}
\end{figure}

A relationship is an association between two entities. For example: Atze is a
contributer to the UHC Haskell compiler. Like entities, relationships can be
grouped into relationship sets. A relationship set can be modeled as a
set containing a tuple for every relationship. The components of the
tuple are the corresponding entities. A relationship can also have attributes,
for example, it might be that the relationship \relationship{contributes} has an
attribute \attrib{since} that records when the author started working on a
compiler. As with entity sets, all relationships in a relationship set share the
same structure. In figure \ref{fig:contributes}, we see an example of the
\relationship{contributes}
relationship set.

\begin{figure}
\includegraphics[width=10cm]{ermodels/contributes}
\caption{The \relationship{contributes} relationship set}
\label{fig:contributes}
\end{figure}

A relationship can be one-to-one, one-to-many or many-to many. For example, an
person might contribute to many compilers, and a compiler might have many
contributors. This is an example of a many-to-many relationship. Next, a
compiler might have multiple releases, but every release belongs to exactly one
compiler. This is an example of a one-to-many relationship. We will call this
property the cardinality of the relationship.

\begin{figure}
\includegraphics[width=16cm]{ermodels/erdiagram}
\caption{An ER diagram about Haskell compilers}
\label{fig:ermodel}
\end{figure}


An ER model is encoded graphically, and describes the entity sets, their
attributes and the relationship sets. An instance of a specific ER model
contains the entities and relationships. Figure \ref{fig:ermodel} is a sample ER
model that represents information about Haskell compilers. In the following section,
we will see how to encode such an ER model in Haskell.

\section{Encoding an ER model in Haskell}

For the sake of simplicity, in this section, we consider a simplified version of
the ER model. We will only have relationships between two types, and no
attributes on relationships. Instead of letting the library user define a key to
identify an entity, we add an integer attribute \attrib{id} for every entity
set. We have not tried encoding a full ER model yet.

To encode an ER model in Haskell, we wil start with
the entity sets. We can encode an entity set $E_1$ as a Haskell type |T_1|. For
example, the entity set Haskell Compilers might be encoded as the datatype
|Compiler|. We will call such a datatype an entity datatype. Every entity
datatype will have a single constructor, which is defined using record syntax,
containing a field for every attribute. As an example, we have encoded the
entity sets figure \ref{fig:ermodel} with their attributes:

> data Compiler  = Compiler  {name :: String, homepage :: URL}
> data Person    = Person    {firstName :: String, lastName :: String, email :: Email}
> data Release   = Release   {title :: String, date :: Date, notes :: String}

An entity in the Haskell Compiler set will be encoded as a Haskell value of type
|Compiler|. For example, the UHC compiler might be encoded as:

> uhc :: Compiler
> uhc = Compiler "UHC" (URL "http://www.cs.uu.nl/wiki/UHC")

Now we will show how to encode relationship sets. As stated, we will limit
ourselves to relationships between two entities, with no attributes for
the relationships. These relationships can be one-to-one, one-to-many or
many-to-many.

As an example, take the \relationship{contributes} in Figure \ref{fig:contributes}.
Ultimately, when interfacing to a database, we might want to query the database
for the contributors of a compiler entity.  In our library, we want to make sure that we get
a collection of contributors, not just one contributor. Also, we want to make sure that
every element in the collection is of the Person entity datatype.

This can be achieved by storing the entitity types and the cardinality of the
relationship set on the type level. For example, we could encode the
\relationship{contributes} relationship set like this:

> data Rel cardL entityL cardR entityR where
>   Rel :: Rel cardL entitityL cardR entityR
>
> contributes :: Rel Many Compiler Many Person
> contributes = Rel

Armt this point, we can define entities, entity sets, relationships and
relationship sets. In order to define an ER model, we will have to link the two
together. However, before we can do that, we have to alter our type |Rel| to be
more restricted. When encoding an ER model, we want to make sure that we will
only encode relationships between entities in the same ER model. In order to do that, we
will define a GADT |phi| that contains a constructor for every entity type. This
is similar to the technique used in the generic programming library multirec
(TODO cite). For our sample model, |phi| looks like this:

> data Entities where
>   PrCompiler  :: Entities Compiler
>   PrPerson    :: Entities Person
>   PrRelease   :: Entities Release

A value of the |Entities| datatype will serve as proof that a type (such as
|Person|) belongs to our ER Model. Now we can add an additional type-parameter
to |Rel|, and two parameters to its constructor, with which we can make sure the
entities are in the same ER Model:

> data Rel (phi :: * -> *) cardL entityL cardR entityR where
>   Rel :: phi entityL -> phi entityR -> Rel phi cardL entitityL cardR entityR

We will also define a typeclass |El| that is instiated for every element in the
GADT:

> class     El  phi       a         where  el  ::  phi a
> instance  El  Entities  Compiler  where  el  =   PrCompiler
> instance  El  Entities  Person    where  el  =   PrPerson
> instance  El  Entities  Release   where  el  =   PrRelease

Now we will also have to change our |contributes| relationship type
appropriately, and also implement our releases relationship:

> contributes ::  Rel  Entities  Many  Compiler  Many  Person
> contributes  = Rel  PrCompiler  PrPerson
> releases    ::  Rel  Entities  One   Compiler  Many  Release
> releases     = Rel  PrCompiler  PrRelease

Now we can finally link the relationship sets and the entity sets to each other.
We enumerate the relationships in a nested tuple:

> type Relations =  (     Rel Entities  Many  Compiler  Many  Person
>                   ,  (  Rel Entities  One   Compiler  Many  Release, ())
>                   )

We define a typeclass with a functional dependency that links the relationships to
the entity sets:

> class ERModel phi rels | phi -> rels, rels -> phi where
>   relations :: rels
>
> instance ERModel Entities Relations where
>   relations = (contributes, (releases, ()))

We now have encoded the ER model from the previous section in Haskell in a
straightforward way. In the next section, we will see how we can automatically
derive a simple in-memory database from our ER model.

\section{Building an in-memory database in Haskell}

From the ER model we can build an in-memory database in Haskell. We want
operations to create, read, update and delete an entity. We want to do the same
things for relationships. Additionally, we want to keep relationships sound. For
example, consider the \relationship{contributes}: we want to make sure that every
release belongs to exactly one compiler. We will start out with storing just
entities, and from there move to storing relationships as well.

To store entities, we will use a |Map| for each entity datatype. The |Map| uses integers
(the primary keys) as keys, and the entities as values. When we create an
entity, the integer key is returned as a reference to that entity. However,
returning just an integer key is a bit too untyped: we also want to encode the
type that we are dealing with. Therefore, we create the a datatype for
references. It is indexed by both the ER model and the entity datatype:

> data Ref phi a where
>   Ref :: phi a -> Int -> Ref phi a

For a given entity datatype |e| we can create a map:

> type EntityMap e = M.Map Int e

However, we have to store such an |EntityMap| for every entity set in our
domain. Consider the list of entity datatypes in our ER model:

> type EntityEnum = (Compiler, (Person, (Release, ())))

Using a type level function (TODO reference), we can now easily determine what
our final structure for storing entities will be:

> type family TList (f :: * -> *)  (phi :: * -> *) enum :: *
> type instance TList f phi ()       = ()
> type instance TList f phi (x, xs)  = (f x, TList f phi xs)
> 
> type EntityStorage phi enum = TList EntityMap phi enum

Before we can build the initial datastructure, we need something on the
value-level that enumerates the types as well, so we introduce a |Witnesses|
datatype as well:

> data Witnesses (phi :: * -> *) (env :: *) where
>   WNil   :: Witnesses phi ()
>   WCons  :: El phi ix => Witnesses phi env -> Witnesses phi (ix, env)

By constructing a value of |Witnesses| with a |WCons| for every type in our ER
model we can make a list of all the proofs.

> witnesses :: Witnesses Entities EntityEnum 
> witnesses = WCons (WCons (WCons WNil))

We now are finally ready to construct our datastructure, containing a |Map| for
every entity datatype.

> emptyStorage :: Witnesses phi enum -> EntityStorage phi enum
> emptyStorage WNil        = ()
> emptyStorage (WCons xs)  = (M.empty, emptyStorage xs)

Our next task is to create entities and add them to the storage. For this, we
will need find the corresponding |Map| for an entity. Consider the following
function:

> newEntity :: El phi ent => ent -> EntityStorage phi enum -> EntityStorage phi enum

In order for this to work, we will need to find the |Map| for the datatype
|ent|. We know that |ent| is in |phi|, because of the typeclass constraint.
However, we don't know at what position in our |EntityStorage| the |Map| for |ent| is. In order to do that, we will
introduce another typeclass that captures the relationship between |phi| and
|enum|. It has an |allTypes| function that builds the |Witnesses|, and a
function that, given a proof that |ix| is in |phi|, gives an index into the
list.

> class EnumTypes phi ls | phi -> ls, ls -> phi where 
>   allTypes :: Witnesses phi ls
>   index :: phi ix -> TIndex phi ix ls

The type |TIndex| is a typed index into the environment. This is the same index
as Baars (TODO cite) uses:

> data TIndex (phi :: * -> *) ix env where
>   Zero  :: TIndex phi ix (ix, env)
>   Suc   :: TIndex phi ix env' -> TIndex phi ix (b, env')

An as an example, here is an instance for the ER model from the previous section:

> instance EnumTypes Entities EntityEnum where
>   allTypes      = WCons (WCons (WCons (WCons WNil)))
>   index User    = Zero
>   index Post    = Suc Zero
>   index Comment = Suc (Suc Zero)

Using a |TIndex|, we can write an update function that changes the map for a
given index:

> modTList  :: (f ix -> f ix) -> TIndex phi ix env -> TList f phi env -> TList f phi env
> modTList f Zero (a,b) = (f a, b)
> modTList f (Suc x) (a,b) = (a, modTList f x b)

And we are now ready to update our storage:

> freshVariable = 0
> 
> newEntity  ::  (EnumTypes phi enum, El phi ent) 
>            =>  ent -> EntityStorage phi enum -> (EntityStorage phi enum, Ref phi ent)
> newEntity e ls = let ident = freshVariable
>                      ls' = modTList (M.insert ident e) (index el) ls
>                      ref = Ref el ident
>                  in (ls', ref)

The fresh variable is now a constant function, but by putting the
|EntityStorage| and a fresh variable counter into a |State| monad, we can get
fresh variables.

For looking up an entity given a reference, we will first need a lookup function
on the |EntityStorage|. Again, we can build a general function for |TList| that
does just this:

> lookupTList :: TIndex phi ix env -> TList f phi env -> f ix
> lookupTList Zero = fst
> lookupTList (Suc x) = lookupTList x . snd

> lookupEntity  ::  (EnumTypes phi enum, El phi ent) 
>               =>  Ref phi ent -> EntityStorage phi enum -> Maybe ent
> lookupEntity (Ref pr ix) = M.lookup ix . lookupTList pr

Saving and deleting are implemented in the same straightforward way.

TODO: relationships.

\section{Saving the in-memory database to a relational database}

\section{Implementation}
%   Describing entities as Haskell datatypes
%   Having typed relations
%   Storing the entities and relations in memory
%     Storing the entities in memory
%     Storing the relations in memory
%   Example: creating a new entity
%     - Filter on relations
\section{Future work}
%   - Extend relationships to have attributes, too.
%   - Extend relationships to be about more than two entities.

\section{Conclusion}

\end{document}
