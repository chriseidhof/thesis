\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
%include forall.fmt 
%format :-> = "\mapsto"
%format +++ = "+\!\!\!+\!\!\!+"
%format << = "<\!\!<"
%format >>> = ">\!\!>\!\!>"
%format <$> = "<\!\!\$\!\!>"
%format phi = "\phi"
%format ~= = "\leadsto"
%format T_1
%format c1
%format c2
%format t1
%format t2
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
\newcommand{\dbtable}[1]{\emph{#1}}
\newcommand{\relationship}[1]{\emph{#1}}
\newcommand{\todo}{{\tiny TODO}}

\hypersetup{colorlinks=true}

\begin{document}
\author{Chris Eidhof}
\title{Describing ER models in Haskell}

\maketitle

\section{TODOs}

\begin{itemize}
\item What about deletion of entities and relationship soundness? 
\item How to update relationships while keeping them sound?
\item How to delete relationships?
\end{itemize}


\section{Introduction}

Entity-relationship modeling \cite{ermodeling} is a tool to design data models.
An entity-relationship (ER) model is a declarative specification of a data
model, and can be used for deriving database schemas. An ER model describes
entities and their relationships. In this chapter, we will see how we can encode
ER models in Haskell. We will derive an in-memory database and a schema for a
relational database from such an ER model.

% TODO: move to section on the end?
Building an interface to database management systems in Haskell is not a new
idea. Leijen (TODO cite) described HaskellDB, a typed interface to relational
databases. Using HaskellDB, the programmer can model relational database schemas
and query the database in a strongly typed way. Our approach is based on ER models,
which work on a higher level than relational databases. A relational database schema
is called a logical data model, whereas an ER model is a conceptual model.

In the next section we give a definition of an ER model and introduce the
vocabulary for ER modeling. In section \ref{sec:encoding}, we show how to
encode an ER model in Haskell.  In section \ref{sec:inmem}, we build an
in-memory database and in section \ref{sec:rdb} we interface to a
relational database. In every section we will build part of a library for
constructing and working with ER models.

\section{ER models}
\label{sec:ermodels}

An entity is an object in a specific problem domain. Examples of an entity are:
the UHC Haskell compiler, the Haskell website or the manager of the Haskell
website. Similar entities are grouped into entity sets. Example entity sets
include: the collection of Haskell compilers, the collection of websites and the
collection of people working on Haskell.

An entity is described using attributes, which map from an entity to a value. Attributes of a
Haskell compiler might be \attrib{name} and \attrib{homepage}. All
entities in an entity set have the same attributes. Every attribute has a
domain, for example: the domain of a compiler's \attrib{name} is the set of all
strings, the domain of a \attrib{release date} is the set of all dates.  For
each entity set, there is a \emph{primary key}, which uniquely identifies an
entity. In Figure \ref{fig:compilers}, we see an example of the Haskell Compiler
entity set.

\begin{figure}
\includegraphics[width=5cm]{ermodels/compiler}
\caption{The Compiler entity set}
\label{fig:compilers}
\end{figure}

A relationship is an association between two or more entities. For example: Atze is a
contributer to the UHC Haskell compiler. 
A relationship can also have attributes, for example, it might be that the
relationship \relationship{contributes} has an attribute \attrib{since} that
records when the author started working on a compiler. A relationship can be
modeled as a tuple, containing an element for every entity and an element for
every attribute.

Like entities, relationships can be
grouped into relationship sets. All relationships in a relationship set have the
same structure, i.e. they relate between the same entity sets and have the same
attributes. Figure \ref{fig:contributes} shows an example of the
\relationship{contributes} relationship set.

\begin{figure}
\includegraphics[width=10cm]{ermodels/contributes}
\caption{The \relationship{contributes} relationship set}
\label{fig:contributes}
\end{figure}

A relationship between two items can be one-to-one, one-to-many or many-to many.
For example, a person might contribute to many compilers, and a compiler might
have many such contributors. This is an example of a many-to-many relationship. On
the other hand, a compiler might have multiple releases, but every release
belongs to exactly one compiler. This is an example of a one-to-many
relationship. We call this property the \emph{cardinality} of the relationship.

\begin{figure}
\includegraphics[width=16cm]{ermodels/erdiagram}
\caption{An ER model of Haskell compilers}
\label{fig:ermodel}
\end{figure}


ER modeling is done graphically, and describes the entity sets, their
attributes and the relationship sets.  Figure \ref{fig:ermodel} is a sample ER
model that represents information about Haskell compilers. 

\section{Encoding an ER model in Haskell}

\label{sec:encoding}

From now on, we will use a simplified version of the ER model. We only consider
relationships between exactly two types. There are no attributes on
relationships.  Instead of letting the library user define a key to identify an
entity, we automatically add an integer attribute \attrib{id} for every entity
set. While these constrains might look tight, in practice they often work very
well. We have not yet looked at supporting full ER models.

To encode an ER model in Haskell, we start with
encoding the entity sets. We can encode an entity set $E_1$ as a Haskell type |T_1|. For
example, the entity set Haskell Compilers might be encoded as the datatype
|Compiler|. We call such a datatype an \emph{entity datatype}. Every entity
datatype has a single constructor containing a field for every attribute. As an
example, we have encoded the entity sets from figure \ref{fig:ermodel}:


> data Compiler  = Compiler  {name :: String, homepage :: URL}
> data Person    = Person    {firstName :: String, lastName :: String, email :: Email}
> data Release   = Release   {title :: String, date :: Date, notes :: String}

An entity in the Haskell Compiler set is encoded as a Haskell value of type
|Compiler|. For example, the UHC compiler might be encoded as:

> uhc :: Compiler
> uhc = Compiler "UHC" (URL "http://www.cs.uu.nl/wiki/UHC")

Before we show how to encode relationship sets, consider the \relationship{contributes} in Figure \ref{fig:contributes}.
Ultimately, when interfacing to a database, we might want to query the database
for the contributors of a compiler entity.  We then want to make sure that we get
a collection of contributors, not just one contributor. Also, we want to make sure that
every element in the collection is of the \entset{Person} entity datatype.
This can be achieved by storing both the entitity types and the cardinality of the
relationship set on the type level. For example, we could encode the
\relationship{contributes} relationship set by constructing a value of the
datatype |Rel|:

> data Rel cardL entityL cardR entityR where
>   Rel :: Rel cardL entitityL cardR entityR
>
> contributes :: Rel Many Compiler Many Person
> contributes = Rel

At this point, we can define entities, entity sets and
relationship sets (note that \relationship{contributes} is a relationship set,
not a relation). In order to define an ER model, we have to combine the entity
sets and the relationship sets. Before we can do that, we change the type |Rel| to be
more restricted: we make sure that we 
only encode relationships between entities in the same ER model. Therefore, we
define a GADT |phi| that contains a constructor for every entity type. This
is similar to the technique used in the generic programming library multirec
\cite{multirec}. For our sample model, |phi| looks like this:

> data Entities e where
>   PrCompiler  :: Entities Compiler
>   PrPerson    :: Entities Person
>   PrRelease   :: Entities Release

A value of the |Entities| datatype serves as a proof that a type belongs to our
ER model. Such a value is also called a \emph{code}.  For example, the code
|PrCompiler| is a proof that |Compiler| is in our ER model.  We
add an additional type-parameter to |Rel|, and two parameters to its
constructor, so that we can ensure the entities are in the same ER Model:

> data Rel (phi :: * -> *) cardL entityL cardR entityR where
>   Rel :: phi entityL -> phi entityR -> Rel phi cardL entitityL cardR entityR


Our |contributes| relationship type has to change appropriately. We also define
the \relationship{releases} relationship.

> contributes ::  Rel  Entities  Many  Compiler  Many  Person
> contributes  = Rel  PrCompiler  PrPerson
> releases    ::  Rel  Entities  One   Compiler  Many  Release
> releases     = Rel  PrCompiler  PrRelease

Now we can finally link the relationship sets and the entity sets to each other.
All relationship sets are enumerated in a nested tuple:

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

We now achieved the goal stated in in the introduction: the ER
model from the previous section is encoded in Haskell. In the next section,
we will build a simple in-memory database from the ground up. If you are
interested in the implementation, continue reading. If you are just interested
in using it, skip to section \ref{sec:inmeminterface} on page
\pageref{sec:inmeminterface} for a description of the interface.


\section{Building an in-memory database in Haskell}
\label{sec:inmem}

From the ER model we can build an in-memory database in Haskell. We want
operations to create, read, update and delete an entity. We want to have the same
operations on relationships. Additionally, we want to keep relationships sound. For
example, consider the \relationship{contributes} relationship: we want to make sure that every
\entset{Release} belongs to exactly one \entset{Compiler}. In section \ref{sec:entities} we will
see how we can store entities and in section \ref{sec:inmemrels} we will see how
we can store relationships. Finally, in section \ref{sec:inmeminterface} we will
build the interface for a library by combining the relationship storage and
entity storage.


\subsection{Storing entities}

\label{sec:entities}

To store entities, we use a |Map| for each entity datatype. The |Map| uses integers
(the primary keys) as keys, and the entities as values. When we create an
entity, the integer key is returned as a reference to that entity. However,
returning just an integer value is a bit too untyped: we also want to encode the
type that we are dealing with. Therefore, we create the a datatype for
references. It is indexed by both the ER model and the entity datatype:

> data Ref phi a where
>   Ref :: phi a -> Int -> Ref phi a

For a given entity datatype |e| we can create a map:

> type EntityMap e = M.Map Int e

We need an |EntityMap| for every entity datatype in our model. Consider the
list of entity datatypes in our ER model:

> type EntityEnum = (Compiler, (Person, (Release, ())))

Using a type level function, we can now easily determine what
our final structure for storing entities is:

> type family TList (f :: * -> *)  (phi :: * -> *) enum :: *
> type instance TList f phi ()       = ()
> type instance TList f phi (x, xs)  = (f x, TList f phi xs)
> 
> type EntityStorage phi enum = TList EntityMap phi enum

As an example, we can manually derive the |EntityStorage| for |Entities| and |EntityEnum|:

>     EntityStorage Entities EntityEnum  
> ~=  TList EntityMap Entities EntityEnum
> ~=  TList EntityMap Entities (Compiler, (Person, (Release, ())))
> ~=  (EntityMap Compiler, TList EntityMap Entities (Person, (Release, ())))
> ~=  (EntityMap Compiler, (EntityMap Person, TList EntityMap Entities (Release, ())))
> ~=  (EntityMap Compiler, (EntityMap Person, (EntityMap Release, TList EntityMap Entities ())))
> ~=  (EntityMap Compiler, (EntityMap Person, (EntityMap Release, ())))

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

The typeclass |El| is instantiated for every element in |phi| and lets the
compiler infer the right code:

> class     El  phi       a         where  el  ::  phi a
> instance  El  Entities  Compiler  where  el  =   PrCompiler
> instance  El  Entities  Person    where  el  =   PrPerson
> instance  El  Entities  Release   where  el  =   PrRelease

We now are finally ready to construct our datastructure, containing a |Map| for
every entity datatype.

> emptyStorage :: Witnesses phi enum -> EntityStorage phi enum
> emptyStorage WNil        = ()
> emptyStorage (WCons xs)  = (M.empty, emptyStorage xs)

Our next task is to create entities and add them to the storage. For this, we
need to find the corresponding |EntityMap| for an entity. Consider the following
function:

> newEntity :: El phi ent => ent -> EntityStorage phi enum -> EntityStorage phi enum

In order for this to work, we need to find the |Map| for the datatype
|ent|. We know that |ent| is in |phi|, because of the typeclass constraint.
However, we don't know at what position in our |EntityStorage| the |Map| for |ent| is. In order to do that, we 
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
>   allTypes        = WCons (WCons (WCons (WCons WNil)))
>   index Compiler  = Zero
>   index Person    = Suc Zero
>   index Release   = Suc (Suc Zero)

Using a |TIndex|, we can write an update function that changes the map for a
given index:

> modTList  :: (f ix -> f ix) -> TIndex phi ix env -> TList f phi env -> TList f phi env
> modTList f Zero     (a,b) = (f a,  b)
> modTList f (Suc x)  (a,b) = (a, modTList f x b)

And we are now ready to update our storage:

> freshVariable = 0
> 
> newEntity  ::  (EnumTypes phi enum, El phi ent) 
>            =>  ent -> EntityStorage phi enum -> (EntityStorage phi enum, Ref phi ent)
> newEntity e ls =  let  ident  = freshVariable
>                        ls'    = modTList (M.insert ident e) (index el) ls
>                        ref    = Ref el ident
>                   in (ls', ref)

The fresh variable is now a constant function, but in section
\ref{sec:inmeminterface}, we will put the |EntityStorage| and a fresh variable
counter into a |State| monad, so we can get real fresh variables.

For looking up an entity given a reference, we first need a lookup function
on the |EntityStorage|. Again, we can build a general function for |TList| that
does just this:

> lookupTList :: TIndex phi ix env -> TList f phi env -> f ix
> lookupTList Zero = fst
> lookupTList (Suc x) = lookupTList x . snd

> lookupEntity  ::  (EnumTypes phi enum, El phi ent) 
>               =>  Ref phi ent -> EntityStorage phi enum -> Maybe ent
> lookupEntity (Ref pr ix) = M.lookup ix . lookupTList pr

Saving and deleting are implemented in the same straightforward way.

\subsection{Saving relationships}
\label{sec:inmemrels}

%include ../packages/Basil/src/Basil/InMemory/Relations/Storage.lhs

\subsubsection{Initial Values}

\label{sec:initialvalues}

When we create a new entity, we want to be sure that all the corresponding
relationships are initialized. For example, in our compilers ER model, whenever
we create a new |Release| entity, we want to be sure that a relationship between
|Compiler| and |Release| is added, because the relationship set
\relationship{releases} describes that every |Release| should be related to
exactly one |Compiler|.

%include ../packages/Basil/src/Basil/Relations/InitialValues.lhs

We are now ready to combine the storing of values and the initial values into a
convenient interface.

%include ../packages/Basil/src/Basil/InMemory/Relations.lhs

\subsection{The library interface}

%include ../packages/Basil/src/Basil/InMemory/Interface.lhs

\label{sec:inmeminterface}


% The approach we take is changing the type of |newEntity| to include all
% neccessary relationships. In our |ERModel| typeclass we have listed all the
% relationship sets in an ER model. Using a function on the type level, we can filter
% out the relationship sets that are interesting for us. Specifically, if there is
% a to-one relationship, we want to include it when creating a new entity.
% 
% If we take a look at our example model, we can easily see how we can derive the
% initial relationships for a ER model. When we add a |Compiler| entity, we do not
% have to add any initial relationship, because a compiler can have a relationship
% with many |Release| and |Author| entities. An |Author| 
% 
% We now add an additional parameter to the |newEntity| function that requests an
% entity reference for every to-one relationship.
% 
% > newEntity  ::  (EnumTypes phi enum, El phi ent, ERModel phi rels) 
% >            =>  ent 
% >            -> FilteredEntities phi rels ent 
% >            -> EntityStorage phi enum -> (EntityStorage phi enum, Ref phi ent)
% 
% The |FilteredEntities| can be implemented using type-functions.

\section{Interfacing with a relational database}

In this section, we build an interface with a relational database.
. In section \ref{sec:rdbschema} we show how we can model a relational database
in Haskell. In
section \ref{sec:rdbentities} we show how we can store entities in a relational
database. Finally, in section \ref{sec:rdbrels} how we can store relationships.
We will end with an easy to use interface for the user in section
\ref{sec:rdbinterface}

\subsection{Modeling a relational database}
\label{sec:rdbschema}

A relational database management system (RDBMS) is a widely used database
system. In an RDBMS, data is stored in tables. Every table has a schema, which
describes what kind of data is stored. Such a schema is a list of attributes
and an attribute consist of a name and a type. A table consists of rows. A row
is a tuple with an element for every attribute.

%include ../packages/Basil/src/Basil/Database/Relational/Core.lhs
%include ../packages/Basil/src/Basil/Database/Relational/Operations.lhs

\subsection{Converting entities}
\label{sec:rdbentities}

%include ../packages/Basil/src/Basil/Database/Relational/Entities.lhs


\subsection{Operations on relationships}
\label{sec:rdbrels}

%include ../packages/Basil/src/Basil/Database/Relational/Relationships.lhs

\subsection{Building an interface}
\label{sec:rdbinterface}

TODO: show how we can have the same interface for relational databases.

\section{Saving the in-memory database to a relational database}

\label{sec:rdb}

When building a text-editor, the workflow of an end-user is often like this: they
load a document, make some changes and once they are happy with their changes,
they store the document. On the implementation level, the text file is first
read into memory, that memory is altered, and finally the memory is written back
to the hard disk.

For more complicated documents, instead of a text-file, we might use a database.
The user will expect the same behavior: they load a file, make some changes, and
the changes are only persisted when they press \emph{save}. By combining our
libraries for the relational database and the in-memory database we can achieve
exactly this: the document is read from the database, changes are stored in the
in-memory database and the in-memory database is finally stored to the
relational database. CoreData \todo{cite} is a proprietary technology by Apple
that, among other things, implements this behavior.

There are two approaches to achieve this using the libraries we have at our
disposal. The first approach would involve extending the in-memory database to
include hooks. For example, there might be a hook that is called whenever an
item is not found (in which case we will try to find the item in a relational
database). However, we propose an approach that is simpler: we build a couple of
top-level functions that can combine two persistance implementations. 

We can define a common interface for persistance layer, which is implemented by
both the in-memory database and the 


\section{Future work}

\begin{itemize}
\item Extend relationships to have attributes, too.
\item Extend relationships to be about more than two entities.
\item More support for (primary) keys.
\end{itemize}

\section{Conclusion}

\end{document}
