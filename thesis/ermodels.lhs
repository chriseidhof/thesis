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

%include formatting.lhs

\begin{document}
\author{Chris Eidhof}
\title{Describing ER models in Haskell}

\maketitle

\tableofcontents

%endif

\section*{TODOs}

\begin{itemize}
\item What about deletion of entities and relationship soundness? 
\item How to update relationships while keeping them sound?
\item How to delete relationships?
\end{itemize}



\section{Introduction}

Entity-relationship modeling \cite{chen1976entity} is a tool to design data models.
An entity-relationship (ER) model is a declarative specification of a data
model, and can be used for deriving database schemas. An ER model describes
entities and their relationships. In this chapter, we show how we to encode
ER models in Haskell in a type-safe way.
We will derive an in-memory database and a schema for a relational database from
such an ER model.

% TODO: move to section on the end?
Building an interface to database management systems in Haskell is not a new
idea.
HaskellDB is a typed interface to relational
databases \cite{bringert2004student, leijen2000domain}.
Using HaskellDB, the programmer can model relational database schemas
and query the database in a strongly typed way.
Our approach is based on ER models and operates on a different level than
HaskellDB. More specifically, data models can be divided into three categories \cite{chen1976entity}:

\begin{itemize}
\item \emph{Conceptual models}, where entities and their relationships are
described on a conceptual level. For example, an ER model.
\item \emph{Logical models}, which are expressed in terms of the database
management system. For example, relationships are expressed using foreign keys
or join tables.
\item \emph{Physical models}, which describe how logical models are stored on
disk or in memory.
Sometimes this is presented to the user.
For example, a schema designer might add indexes on keys, which directly
corresponds to changes in the physical layer.
\end{itemize}

ER-models fall into the category of \emph{conceptual models}, whereas HaskellDB
falls into the category of \emph{logical models}.
Recent work by Visser \cite{sebasmscthesis}
operates on the physical model.

In section \ref{sec:ermodels}, we give a definition of an ER model and introduce the
vocabulary for ER modeling. We give an ER model that we will use as our running
example.

In section \ref{sec:encoding}, we show how to encode an ER model in Haskell. 
We do this in a type-safe way. Entities can only have relationships with
entities in the same ER model, and relationships can only relate between two
entities in the ER model. This section encodes a \emph{conceptual model}.

In section \ref{sec:inmem}, we build an in-memory database. Because different
relationships (one to many, many to many), have different ways of handling them,
we make heavy use of type-level programming to keep our code correct. 
This section encodes a \emph{logical model}, and a translation from the
conceptual model to the logic model.

In section \ref{sec:rdbschema} we sketch the outline of an interface to a
relational database. Not all code has been written yet.

Section \ref{sec:rdb} shows how we can combine the
in-memory database and the relational database, where the in-memory database
acts as a \emph{scratch pad} before data is stored.

Section \ref{sec:query}
describes a query language for ER models and how that is translated into queries
for the in-memory database and SQL queries for the relational database.

Finally, in section \ref{sec:erconclusion} we look at how our library works in
practice and discuss future work.

This chapter provides the following contributions:

\begin{itemize}
\item We give an encoding of ER models in Haskell.
\item We translate ER models into an in-memory database.
\item We outline how to translate an ER model into a relational databes.
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

%include ermodels/encoding.lhs

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
as type-level values.

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

Finally, we will provide a function to lookup a value in a list, given an index:

> lookupTList :: Ix phi ix -> HList phi -> ix
> lookupTList Zero     (Cons y ys) = y
> lookupTList (Suc x)  (Cons y ys) = lookupTList x ys

We will provide more functions for our |HList| type that we will introduce when
we need them.

\subsection{Storing entities}

\label{sec:entities}

%include ermodels/entities.lhs

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

%include ../packages/Basil/src/Basil/Relations/PList.lhs

%include ../packages/Basil/src/Basil/InMemory/Relations.lhs

\subsection{The library interface}

%let query = False
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

TODO: show how we can have almost the same interface for relational databases.

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
top-level functions that can combine two persistence implementations. 

\subsection{A common interface for persistence}

The in-memory database shares almost the same interface with the relational
database. We can build a typeclass that abstracts over both interfaces. This
will be very handy when combining the two interfaces in the next section. 

Recall the type for the |new| and |find| operation on the in-memory database:

> find :: (El phi entity) => Ref phi entity -> Basil phi env rels (Maybe entity)
> new  ::  (El phi entity) 
>      =>  entity 
>      ->  PList phi entity (InitialValues phi entity rels rels) rels 
>      ->  Basil phi env rels (Ref phi entity)

%include ../packages/Basil/src/Basil/Interface.lhs

\subsection{Combining the in-memory database and the relational database}

\todo{show how we can combine two arbitrary instances of persistence typeclass
to provide core-data behavior}

\section{Querying the database}
\label{sec:query}

In the previous sections, we saw how we can find all entities in an entity set
or a single entity, by its |id|. In this section we will implement
\emph{selection}, which gives us more advanced ways of finding entities. For
example, we might want to find all |Person| entities with the name |"chris"|, or
all compilers with a version larger than |1|. In this section, we will show how
we can construct such queries in a typed way, and perform them on both the
in-memory database and the relational database. We see querying databases as a
a separate aspect: not every storage engine might support it. For example, we
could use ER models for modeling a webservice, but most webservices don't
support any queries at all. This code is inspired by HaskellDB \todo{cite}

\subsection{Representing queries}

%include ../packages/Basil/src/Basil/Query.lhs
%include ../packages/Basil/src/Basil/QueryExample.lhs

\subsection{Querying the in-memory database}

%let query = True
%include ../packages/Basil/src/Basil/InMemory/Interface.lhs

\subsection{Querying the relational database}

TODO: compile query into sql using |toSql|.

\section{Future work}

\begin{itemize}
\item Extend relationships to have attributes, too.
\item Extend relationships to be about more than two entities (or show how those
can be modeled using only binary relationships)
\item More support for (primary) keys.
\item Undo/redo support built on top of the in-mem/rdbms combination
\item Interface/generate webservice based on ER model
\item Composing of relations? Just like function composition
\end{itemize}

\section{Conclusion}
\label{sec:erconclusion}

%if not thesis

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}

%endif
