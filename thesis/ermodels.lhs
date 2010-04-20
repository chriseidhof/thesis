%if not thesis
\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
%include forall.fmt 
%include chris.fmt 
% vim:spell
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}
\usepackage{hyperref}
\usepackage{todonotes}
\usepackage{natbib}

%include formatting.lhs

\begin{document}
\author{Chris Eidhof}
\title{Describing ER models in Haskell}

\maketitle

\tableofcontents

%endif

\section{Introduction}

Every computer program manipulates data. To describe the data model of a
program, several techniques exist.
In this chapter, we look at one such technique: entity relationship
modeling \cite{chen1976entity}, a way to describe data models on a \emph{conceptual
level}. A data model on the conceptual level, called a conceptual model, closely
matches the way we think about data models.

An entity-relationship model, or \emph{ER model}, is the result of entity relationship
modeling. An entity-relationship model is a conceptual model.
It is a formal description of a data model, which can be used for
various purposes: it is a means to communicate between designers and users of a
system to verify the mutual understanding of a data model.
Furthermore, because an ER model is a formal specification, it can be used for
code generation.
Although an ER model is often manually translated to a database schema, we 
build a library that performs this translation automatically.

In ER models, entities are elementary units, and are linked to each other using
relationships.
An ER model constrains which types of entities can occur in our data model, and
which types of relations can occur.
We encode these constraints in Haskell, using the type system.
The result of our work is a library that allows us to describe an ER model
in Haskell, where all constraints are automatically checked by the type checker.

From a data model on the conceptual level, we can derive a data model on the
\emph{logical level}.
A logical model describes a data model in terms of a database system.
For example, a schema of a relational database system is a logical model, and a
schema of an in-memory database too.
We use our library that encodes ER models to derive logical models
automatically.
For example, we derive an in-memory database schema and a relational
database schema from an encoded ER model.
We leverage the type system to make sure the generated logical models are
correct and have the same constraints as their conceptual counterparts.

The conceptual and logical models are two categories. In \citet{chen1976entity},
three categories for data models are defined:

\begin{itemize}
\item \emph{Conceptual models}, where entities and their relationships are
described on a conceptual level, which matches the way we think about data
models. A conceptual model can be used for both communication as well as a
formal method to describe data models.
\item \emph{Logical models}, which are expressed in terms of the database
management system. A logical model is often derived from the conceptual model.
\item \emph{Physical models}, which describe how logical models are stored on
disk or in hardware memory. A physical model describes the arrangement of the
data on disk.
Sometimes part of this model presented to the user, for example, a schema
designer might add indexes on keys, which directly corresponds to changes in the
physical layer.
\end{itemize}

If we look at related work in Haskell, we are not aware of other work that
operates on the conceptual level. On the logical level, HaskellDB 
\cite{bringert2004student, leijen2000domain} is a typed interface to relational
databases. Recent work by Visser \cite{sebasmscthesis} operates on the \emph{physical
level}.

\todo{revised until here.}
In section \ref{sec:ermodels}, we build an the definition of an ER model and introduce the
vocabulary for ER modeling. We show an ER model that we use as our running
example.

In section \ref{sec:encoding}, we encode an example ER model in Haskell. 
We do this in a type-safe way: entities can only have relationships with
entities in the same ER model, and relationships can only relate between two
entities in the ER model.

In section \ref{sec:inmem}, we build an in-memory database, which is a
\emph{logical model}. We show how to use type-level programming in order to enforce the constraints
defined in the ER model.  The in-memory database is derived from the ER model.

In section \ref{sec:relationaldb} we build an interface to a relational database.
Again, we use the ER model to derive the relational database schema.

Section \ref{sec:query}
describes a query language for ER models and how that is translated into queries
for the in-memory database and SQL queries for the relational database.

Finally, in section \ref{sec:erfuture} we describe future work, and in section
\ref{sec:erconclusion} we conclude. To be complete, we have included all the
interfaces of the libraries in section \ref{sec:erinterfaces}.

This chapter provides the following contributions:

\begin{itemize}
\item We give an encoding of ER models in Haskell.
\item We translate ER models into an in-memory database.
\item We translate ER models into a relational databes.
\item We show how we can layer different logical models on top of each other.
\item We define a query language that works on conceptual models and translates
to logical models.
\item Finally, we provide a large example of type-level programming.
\end{itemize}

\section{ER models}
\label{sec:ermodels}

An \emph{entity} is an object in a specific problem domain. Examples of an entity are:
the UHC Haskell compiler, the Haskell website or the manager of the Haskell
website. Similar entities are grouped into \emph{entity sets}. Example entity sets
include: the collection of Haskell compilers, the collection of websites and the
collection of people working on Haskell.

An entity is described using \emph{attributes}, which map from an entity to a value. Attributes of a
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
\label{sec:erencoding}

%include ermodels/encoding.lhs

\section{Building an in-memory database in Haskell}
\label{sec:inmem}

From the ER model we can build an in-memory database in Haskell. We want
operations to create, read, update and delete an entity. We want to have the same
operations on relationships. Additionally, we want to keep relationships sound. For
example, consider the \relationship{contributes} relationship: we want to make sure that every
\entset{Release} belongs to exactly one \entset{Compiler}. In section \ref{sec:entities} we 
see how we can store entities and in section \ref{sec:inmemrels} we see how
we can store relationships. Finally, in section \ref{sec:inmeminterface} we 
build the interface for a library by combining the relationship storage and
entity storage.

\subsection{Using type-level programming libraries}

We have experimented with using the |HList| library for this module, but that
turns out to be quite inconvenient. It is especially different to compose
functions such as |hMap| and |hLookupByHNat|.
From our own experience, and by talking to other people that use type-level
programming libraries, it turns out that almost everybody uses their own
type-level programming library.
We think this is not a deficit of the libraries, but a language problem.
The first problem is that a library is sometimes not expressive enough.
A second, more general problem, is that type errors become very large.
Unfortunately, we cannot always hide the type errors from our library users, so
they have to deal with the internals of the libary too.

Therefore, we implement our own heterogenous lists. An |Hlist| is simply defined
as either a |Nil| or a |Cons| value. They produce an index that is either |Nil|
or |:*:|, which are both defined as empty datatypes because they are only used
as type-level values. The full interface for our |HList| library is defined in
section \ref{sec:erhlist}.

> data HList a where
>   Nil  :: HList Nil
>   Cons :: a -> HList b -> HList (a :*: b)
>
> data (:*:) a b
> data Nil

We can also provide typed indexes into the |HList|, which are inspired by
Baars and Swierstra \cite{tttas}:

> data Ix ls ix where
>   Zero  :: Ix (a :*: b) a
>   Suc   :: Ix xs a -> Ix (x :*: xs) a

\subsection{Storing entities}

\label{sec:entities}

%include ermodels/entities.lhs

\subsection{Saving relationships}
\label{sec:inmemrels}

In this section, we store relationships in our in-memory database. 
To do this, we have to compute the right data-structure based on the
relationship type.

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

%include ../packages/Basil/src/Basil/Relations/PList.lhs

%include ../packages/Basil/src/Basil/InMemory/Relations.lhs

\subsection{Constructing the library interface}
\label{sec:inmeminterface}

%let query = False
%include ../packages/Basil/src/Basil/InMemory/Interface.lhs


\subsection{In-memory example}
\label{sec:inmemexample}

%include ermodels/example.lhs

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
\label{sec:relationaldb}

In this section, we build an interface with a relational database.
In section \ref{sec:rdbschema} we show how we can model a relational database
in Haskell. We use a typed approach that is inspired by Leijen
\cite{leijen2000domain}, and Oury and Swierstra \cite{oury2008power}.
In section \ref{sec:rdbentities} we show how we can store entities in a relational
database, this is a simple translation.
Section \ref{sec:rdbrels} describes how we can store relationships. In
particular, we show how to translate relationships to either foreign keys
or join tables.
Finally, we end with an easy to use interface for the user in section
\ref{sec:rdbinterface}.

\subsection{Modeling a relational database}
\label{sec:rdbschema}

A relational database management system (RDBMS) is a widely used type of database
systems.
In an RDBMS, all data is stored in tables.
Every table has a schema, which
describes the structure of the data.
Such a schema is a list of attributes, where an attribute consist of a name and a type.
Every table stores a list of rows, where each row is a tuple with values for the
attributes in the schema.
A large difference with ER models is that there is no notion of relationships in a
RDBMS, instead a relationship is encoded by adding tables, which we expand upon in
section \ref{sec:rdbrels}.

%include ../packages/Basil/src/Basil/Database/Relational/Core.lhs
%include ../packages/Basil/src/Basil/Database/Relational/CoreExample.lhs
%include ../packages/Basil/src/Basil/Database/Relational/Operations.lhs

\subsection{Converting entities}
\label{sec:rdbentities}

%include ../packages/Basil/src/Basil/Database/Relational/Entities.lhs


\subsection{Operations on relationships}
\label{sec:rdbrels}

%include ../packages/Basil/src/Basil/Database/Relational/Relationships.lhs

\subsection{Building an interface}
\label{sec:rdbinterface}

For relational databases, we have provided an interface that is almost the same
as the in-memory database. Instead of having |Basil| as the type, the relational
database have |BasilDB| as their type. This allows us to build an application
and test it using the in-memory database, and switch to a relational database
for production use. The full interface is described in section
\ref{sec:reldbinterface}.

\section{Querying the database}
\label{sec:query}

In the previous sections, we saw how we can find all entities in an entity set
or a single entity, by its |id|. In this section we implement
\emph{selection}, which gives us more advanced ways of finding entities. For
example, we might want to find all |Person| entities with the name |"chris"|, or
all compilers with a version larger than |1|. In this section, we show how
we can construct such queries in a typed way, and perform them on both the
in-memory database and the relational database.

Querying databases is a a separate aspect: not every logical layer might support it.
For example, we could use ER models to model a remote XML-based webservice, but
most webservices do not support any queries at all. We have implemented querying
for both the in-memory database and the relational database.

\subsection{Representing queries}

%include ../packages/Basil/src/Basil/Query.lhs
%include ../packages/Basil/src/Basil/QueryExample.lhs

\subsection{Querying the in-memory database}

%let query = True
%include ../packages/Basil/src/Basil/InMemory/Interface.lhs


\section{Future work}
\label{sec:erfuture}

Our library provides a good starting point, but is far from complete. In this
section we discuss possible ways to extend our library.

\subsection{ER model extensions}

Our way of representing ER models is quite limited. We can extend our approach
in a couple of directions:

\begin{itemize}
\item \emph{Extend relationships to have attributes}. At the moment,
relationships do not have attributes. By adding support for this we can express
more relationships. Most attributes on relationships can be expressed by moving
the attribute to one of the entities. However, in a many-to-many relationship
this is not possible. 

For example, consider a |Person| who has been working on a
|Compiler|, and we want to track at what date she started working on the
|Compiler|. A way to work around this limitation is by manually introducing a virtual entity |V| that
only has a date attribute, and introduce a one-to-many relationship between 
|V| and |Compiler| and a one-to-many relationship between |V| and |Person|. 

\item \emph{Extend relationships to be between more than two entities}. 
In our current library, we can create a virtual entity to simulate relationships
between more than two entities, but this is cumbersome and requires more work
from the user of the library.

\item \emph{More support for (primary) keys}
Our primary keys are implicit: we add an |id| attribute of type |Int| for each
entitity.
In practice this works well, but more control over primary
keys is sometimes necessary. 

\item \emph{More control over the logical layer}
It can be useful to provide more control over the logical layer. For
example: it is not possible to use our library with legacy databases. When a
schema does not exactly match the schema we generate, our approach does not work
anymore.
RBMSs also provide efficient ways to compose queries (e.g. using the
\texttt{JOIN} statement.) Our library does not compile such composed queries to
efficient \texttt{JOIN} statements, but naively joins tables, resulting in
performance loss. Compiling queries more efficiently would possibly lead to
large performance gains, but in our current implementation this is not possible.

\item \emph{More control over the physical layer}
When building a high-performance application it can be useful to have control
over the physical layer. For example, when a |Person| is often found by
searching for an e-mailaddress, adding a |Trie| datastructure that maps
emailadresses to |Person| values increases performance. Our library could be
extended with configuration options similar to performance pragmas.

\end{itemize}


\subsection{Saving the in-memory database to a relational database}

\label{sec:rdb}

Consider a user that uses a text-editor to edit some files on disk.
The workflow of the user is often like this: she
loads a document and make some changes to document.
Once she is happy with the changes, she stores the document.
If we look at the implementation, the text file is first
read into memory, the memory representation is altered,
and finally the memory is written back to the disk. In other words: the
in-memory database is used as a scratchpad before commiting changes to the real
database.

For more complicated documents, we might use a lightweight database instead of a text-file.
The user expects the same behavior: she loads a file, makes some changes, and
only when she presses \emph{save} the changes are persisted.
By combining our
libraries for the relational database and the in-memory database we can achieve
exactly this: the document is read from the database, changes are stored in the
in-memory database and the in-memory database is finally stored to the
relational database.
This technique is implement by
CoreData\footnote{\url{http://developer.apple.com/macosx/coredata.html}}.

Instead of providing a way to combine the in-memory database and relational
database, we can build a common interface for both databases.  We could then
design a stacking mechanism to join two databases into a new database, which can
then be stacked again.  Another use case would be to have a distributed
databases that stores its data over multiple servers, which we could edit using
an in-memory database.

\subsection{Miscellaneous}

For all operations that alter the data in a database, we could add inverse
operations. This way, we can provide the user of the library with automatic undo
and redo support.

Based on our ER model, we could generate a complete interface for editing values
in databases. This can work independently of the logical layer that is used.
Furthermore, instead of defining our ER model in Haskell, we could provide a
graphical user interface that allows for construction of new ER models.
If we combine these two techniques we can build a platform for rapid application
development (however, unless we provide customization capabilities, the applications
that can be built are quite limited).

\subsection{Improving type errors}

There is a lot of work to be done on improving the type errors of this library.
Because we make heavy use of type-level programming, type errors can grow quite
large and become hard to understand.

To give an example of a simple
mistake: recall the in-memory example from section \ref{sec:inmemexample}. If we
forget to include the |Compiler| entity when creating a |Release| entity, we get the
type error in figure \ref{fig:typeerror}. 
This is an error that is easy to make and it happens quite often in practice.
However, the type error is so complicated that new users of the library might be
scared away instantly. 
From the type error, it is hard to see how the program has to
be changed to be correct.

We would like to use the techniques described by Heeren et al. \cite{heeren2003scripting} to
improve the error messages in our library. However, the technique lacks an
implementation in a mainstream compiler.

\begin{figure}
\begin{verbatim}
[1 of 1] Compiling Example          ( example.lhs, interpreted )

example.lhs:111:52:
    Couldn't match expected type 
           `Basil.Data.TList.R:AppendIfTrueTruexxs
             (Ref (Compiler :*: (Person :*: (Release :*: Nil))) t,
              Dir R,
              Ix
                (TContributes :*: (TReleases :*: Nil))
                (Rel
                   (Compiler :*: (Person :*: (Release :*: Nil)))
                   One
                   Compiler
                   Many
                   Release))
             t1'
           against inferred type `Nil'
      Expected type: 
        List
         CompilerModel
         Release
         (InitialValues
            CompilerModel Release CompilerRelations CompilerRelations)
         CompilerRelations
      InferPList CompilerModel Release Nil CompilerRelations
    In the third argument of `new', namely `PNil'
    In a stmt of a 'do' expression: rId <- new ixRelease ghc612 PNil
Failed, modules loaded: none.
\end{verbatim}
\caption{Forgetting to include a |Ref| to the |Compiler| entity}
\label{fig:typeerror}
\end{figure}


\section{Conclusion}
\label{sec:erconclusion}

In this section we have seen how to translate an ER model into a Haskell
representation.
By making heavy use of type-level programming, we have provided an interface
that is type-safe: all operations on entities and relations are type-checked.
We have seen how to build an in-memory database from our ER model and how to
interface with a relational database.
Our approach abstracts over the logical layer, which allows library users to write code
that works independently of the storage mechanism.

\newpage

\section{Library interfaces}

\label{sec:erinterfaces}

\subsection{HList library}

\label{sec:erhlist}

%include ermodels/hlistinterface.lhs

\subsection{Basil Core Interface}

\label{sec:ercoreif}

%include ermodels/coreinterface.lhs

\subsection{Query Interface}

\label{sec:erqueryif}

%include ermodels/queryinterface.lhs

\subsection{In-memory Interface}

\label{sec:inmemif}

%include ermodels/inmeminterface.lhs

\subsection{Relational database interface}
\label{sec:reldbinterface}

%include ermodels/reldbinterface.lhs


%if not thesis

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}

%endif
