\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
%format :-> = "\mapsto"
% vim:spell
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}

\begin{document}
\author{Chris Eidhof}
\title{Thesis Proposal: The Pesto framework \\
\normalsize{Building generic web interactions in Haskell}
}

\maketitle
\tableofcontents

\section{Introduction}

In this thesis proposal we will describe the building of a workflow management
system implemented in Haskell using domain specific languages. The next section
will give an introduction to workflows and workflow management tools. We will
then give an introduction to domain specific languages and generic programming.
These sections will form the basis for our work, which we will define in
section \ref{approach}.
%TODO: persoonlijke motivatie.

 
\section{Workflows}

Workflow systems describe how humans and computers interact. A \emph{workflow process
definition} describes which activities are needed to complete a certain business
process. These activities can be either human (for example, filling in a form,
confirming a message) or done by the computer (storing a record to the database,
calculating a result). A workflow process definition not only specifies the
activities, but also in what order they need to be executed.

A workflow management system is a tool to build executable specifications of
workflows. It is often a standalone product, and sometimes integrated into a
programming language. This distinction can make a large difference in the
\emph{expressive power} of a workflow management system.

The Workflow Patterns initiative has built a set of commonly recurring patterns
for workflow processes. They are similar to design patterns used in software
development. Not all workflow management systems supports all patterns but
sometimes, depending on the expressive power of such a system, it can be
extended to support additional patterns.

Sometimes, when designing workflow applications, the workflow is implemented
without a workflow management system. For example, it can be implemented in an
ad-hoc way as a web application. This can make it very hard to support
additional platforms, such as mobile devices. By using the right workflow
management system an implementor can abstract over platforms.

Also, a workflow system has to interact with the user and other systems or
processes. Much of this interaction is based on the domain of the problem. In
section \ref{domaindriven} we will see how a domain model can help keeping
workflow applications flexible.

% implementation approaches
%   what's already there? what are the (dis)advantages?

\section{Domain specific languages}

A \emph{domain specific language} (DSL) is a programming language designed to express
solutions in a specific domain. It is the opposite of a general-purpose
programming language. A DSL can be designed to help model a certain class of
problems more directly. At the very minimum, a DSL consists of a set of
\emph{symbols} and a set of rules (syntax) that describe how to combine these
symbols into well-formed expressions.

Symbols in a DSL can take many forms. For example, the syntax might closely
resemble a programming language. Alternatively, in workflow languages, the
modeling of the workflow is often done graphically.  The symbols are the
elements used to draw a workflow graph.

The solution expressed using a DSL can be used for several purposes. Often, it
is given a semantics by interpreting it or compiling it into executable code.
Also, a tool can check for well-formedness (with regard to the syntax) and
correctness (with regard to the semantics) of the expressed solutions.

\subsection{Embedded domain specific languages}

When a DSL is integrated into another language, it is called an \emph{embedded
domain specific language}. Here, the symbols of the DSL are also symbols in the
\emph{host language}. By embedding the DSL into another language, the DSL will
inherit all kinds of properties of that language. 

Using all the features of the host languages gives a DSL a great amount of
interoperability with other libraries. Also, by using the type system of the
host language, a partial correctness check can be done by modeling logical rules
in the type system.  We will elaborate on this in later sections.

As a good example, in the Haskell programming language, there are several
libraries for describing parsers. These libraries provide a number of symbols
that can be combined to write parsers. They use Haskell's type system to give
partial proofs about what the result of parsing might be.

\section{Model Driven Development}
\label{domaindriven}

The domain model describes the entities and relations in a problem domain. In
model driven development, the domain model is kept close to concepts in the
problem domain, in contrast to computing concepts. This again opens up
possibilities for non-technical people to understand part of the application
development.

As new insights arrive from communicating with the stakeholders in the
application development process, the model changes. From our experience,
we know that it takes a couple of iterations before the domain model arrives at
its final state. Therefore, we believe that it is vital to keep the domain model
as flexible as possible.

\subsection{Generic Programming}

TODO: explain nominative vs. structural.

Using datatype-generic programming we can write functions that operate on the
structure of data. A generic function is a function that operates on an
arbitrary value as long as it can be generically inspected. In a language with a
nominative type system, generic programming often does not come for free. A
value first needs to be converted into a structural view before it can be
processed by generic functions. Afterwards, it needs to be converted back. An
\emph{embedded projection pair} is the pair of functions that does these
conversions.

Generic programming can be used to keep the domain model flexible. If we specify
our domain model as nominative types and derive the embedding projection pair,
we can do generic programming on the domain model. We can build a lot of useful
generic functions: a generic database interface, generic forms, XML generation,
to name a few. 

We are not the first to envision this. In recent research, iData has done this
in the Clean programming language. iData is a library of generic functions that
generates forms and offers serialization (in files or a database) of values.
However, it is not clear if it is easy to write custom functions.

\section{Research Statement}


\section{Our approach}
\label{approach}

We are going to build two embedded domain specific languages in the Haskell
programming language. The first EDSL will be about the control flow of workflow
tasks. To further assist building workflow applications, we will build a library
for building domain models. We will provide a structured view on domain models
and build generic functions to deal with them. Before we describe the actual
libraries, we will first discuss some relevant issues when building EDSLs in
Haskell. We will then introduce the two EDSLs and finally discuss the porting of
an existing application to our system.

\subsection{Writing libraries as EDSLs}

When implementing embedded domain specific languages there are a lot of design
choices to be made. For example, when choosing Haskell as a programming
language, you get a lot of things for free, such as automatic sharing of values
and a type system. We will discuss the problems that arise when using sharing
and how we can use Haskell's type system to our advantage. Finally, we will
discuss abstraction for stateful computations.

\subsubsection{Observable Sharing}

Due to the laziness of Haskell it is possible to construct values that can be
infinitely unfolded. For example, consider the following Haskell expression:

\begin{code}
data Graph = Branch Graph Graph | Leaf Int
infiniteGraph = Branch (Leaf 1) infiniteGraph
\end{code}

A graphical representation looks like this: (TODO: insert graph)

The expression |infiniteGraph| is a finite representation of an infinite value.
When analyzing the value, we can endlessly keep unfolding the recursive call to
|myTree|. An alternative way of representing this graph is as following:

\begin{code}
newtype Ref    =  Ref Int -- A reference to a graph expression
data Graph     =  Branch Ref Ref | Leaf Int
infiniteGraph  =  [(Ref 1, Branch (Ref 2) (Ref 1))
                  ,(Ref 2, Leaf 1)
                  ]
\end{code}

Here, we have made the sharing explicit. However, this method has the
disadvantage of being a lot more verbose, and not as safe. It is possible to
construct a Graph expression that refers to a non-existing graph.

Sometimes, we want to specify our graphs using ordinary recursion, as in the
first example, but we would like to be able to convert them into the second
representation, where recursion and sharing is made explicit. By using
techniques for observable sharing we can achieve this. It has first been
introduced to the Haskell community by Claessen and Sands \cite{sharing} and has
been refined over the years. The solution proposed by Gill \cite{reify} has
virtually no impact on the user of the DSL. (TODO: maybe elaborate on the
different papers?)

\subsubsection{Lifting on Haskell's type system}

In this paragraph we will show how advanced features of Haskell's type system
can be used to make stronger guarantees about the correctness of our programs.
For example, when designing an embedded expression language, we could use the
following approach:

\begin{code}
data Exp = Num Int | Boolean Bool | If Exp Exp Exp | Add Exp Exp
\end{code}

However, using this datatype, it is possible to construct an expression that
adds two Booleans. In general, this is not desirable. We would like to prove
statically that every expression is well-typed. By using GADTs \cite{gadts}, 
we can restrict the expressions to be well-typed:

\begin{code}
data Exp a where
  Num      :: Int       -> Exp a
  Boolean  :: Bool      -> Exp Bool
  If       :: Exp Bool  -> Exp a    -> Exp a -> Exp a
  Add      :: Exp Int   -> Exp Int  -> Exp Int
\end{code}

This expression datatype gives a lot more guarantees about the well-typedness of
its expressions. GADTs can help both the designer and user of an EDSL to
guarantee correctness of the EDSL and the programs written with it.

On a more theoretical note, this relates to the Curry-Howard correspondence. For
example, the |If|-constructor can be read as: if you give me an expression that
is proven to be of type |Bool| and two expressions of type |a|, I will construct
an expression of type |a|. The program serves as proof for the type-correctness
of the expressions.

\subsubsection{Control abstraction}

When describing stateful computations in Haskell there are a number of
abstraction mechanisms available. The most commonly used abstractions are
arrows, monads and applicative functors (or idioms). When implementing an EDSL
that does stateful computation, it can be interesting to use one or more of
these abstractions.

The three abstractions in the previous paragraph are not equally powerful. On
his website, Cooper \cite{cooperblog} gives a very clear example of the
the relative strengths. When using monads you can use an intermediate result at
any future step:

\begin{code}
do  x <- compOne
    y <- compTwo
    z <- compThree x
    return (x + y + z)
\end{code}

However, when using arrows, you can only use every intermediate result in the
step that's directly following. By using arrow combinators it is still
possible to thread values through to the next expression, but this has to be
done explicitly.

\begin{code}
do  x <- compOne
    y <- compTwo x
    return y -- It is not allowed to use 'x' here.
\end{code}

Applicative functors restrict this even further, you can only use intermediate
results in a final return clause:

\begin{code}
do  x <- compOne
    y <- compTwo
    return (x + y)
\end{code}

Choosing one of these abstractions can have great impacts on library design, as
we will see in the next paragraph.

\subsection{Control flow: a library for workflow control-flows}

When writing down a workflow, we could do that in a monadic style. For example,
consider the following program:

\begin{code}
example = do  name  <- getName
              email <- lookupEmailInDatabase name
              display  ("Hi, " ++ email ++ ".")
              return   name
\end{code}

This workflow presents the user with a task to enter her name, then a task to
find the correspond e-mail address. Next it displays the e-mail address and it will
return the name. These tasks are sequenced, and not every line requires user
interaction. This is a typical interleaving of human tasks and computer tasks as
can be found in workflows.

If we rewrite this example without |do|-notation, we get the following code:

\begin{code}
example  =    getName 
         >>=  \name  ->  lookupEmailInDatabase name
                         >>= \email ->  display ("Hi, " ++ email ++ ".")
                                        >> return name
\end{code}

It can be seen from this example that the scope of the |name| variable extends
to the rest of the expression. 

Translating this program to a web application is not straightforward. TODO:
explain about statelessness, keeping the program in memory and serializing.


\begin{itemize}
\item Talk about observable sharing.
\item Talk about curry-howard?
\item Talk about continuations/serializing with regard to control abstraction.
\end{itemize}
\subsection{Domain model: generic programming}

For our domain model, we will use generic programming. What we describe in this
section is also known as an entity-relationship model. It describes the
different entities and their relationships. An entity is related to an
entity in the problem domain that we are describing.  Every entity has a number
of attributes.

For example, when modeling an organization, one of the entities might be
|Employee|, which describes an employee in the organization. It can have
attributes such as |name|, |address| and |birthdate|. Another entity might be
|Department|. Typically, an employee belongs to exactly one department but a
department might have any number of employees.

In Haskell, we could define our domain model like this:

\begin{code}
data Employee   = Employee   {name :: String, address :: String, birthdate :: DateTime, department :: BelongsTo Department}
data Department = Department {departmentTitle :: String, employees :: HasMany Employee}
\end{code}

Here, we have explicitly encoded the relationships using special types such as
|BelongsTo| and |HasMany|. TODO: explain why or forward-reference.

\subsubsection{Structural representation}

In order to write generic functions on our domain model, we need a structured
representation for the model and embedding projection pairs. The combination of
these codes and conversion functions is called a \emph{universe}. For our
purposes we will only consider single-constructor record types (todo: explain
why). Therefore, we can restrict our universe to these types. 

TODO: give a more formal definition.

Every generic programming library has different requirements. Rodriguez et al.
(TODO: cite) have compared a large number of different libraries for generic
programming. From carefully analyzing the different criteria in that research we
have chosen to develop a variant of the EMGM \cite{emgm} library. We believe it
is important to have extensible universes so that the programmer is not limited
to the datatypes in the library.

While EMGM is applicable for virtually every kind of datatype, we have chosen a
universe that is specific for our domain model. TODO: explain differences.

\subsubsection{Generic database access}

\subsubsection{Generic forms}

\subsection{Porting an existing application}


% TODO:
% research statement?
% 

%----------------------------------------

% To give a concrete example, a workflow might look like this (written down in Haskell):
% 
% 
% \begin{code}
% \end{code}
% 
% This workflow presents the user with a task to enter her name, then a task to
% find the correspond e-mail address. Next it displays the address and it will
% return the name. These tasks are sequenced, and not every line requires user
% interaction. This is a typical interleaving of human tasks and computer tasks as
% can be found in workflows.
% 
% Building executable specifications is what workflow modeling tools are all
% about. However, execution should not be limited to just one platform, such as the web
% .  We envision a system where a workflow is an abstract description of a
% business process, and not mixed with implementation-specific code such as HTML
% fragments. For example, as mobile phones get more advanced, it would make a lot
% of sense to have a mobile version of a workflow application next to a web
% application. In other words, the workflow specification has to be
% \emph{platform-agnostic}.
% 
% When designing an application that contains the workflow of an organization,
% there are multiple stakeholders. Not all of these people might know how to read
% a specification in the form of a computer program. On the other hand, the
% implementor might have to deal with unclear or incomplete requirements. From our
% own experience, we know that a lot of misunderstanding can be prevented by
% integrating all stakeholders in the development process. In a
% workflow system, workflows can be modelled as graphs and thus have a direct
% visual representation. By drawing this graph automatically from the
% specification there is always an up-to-date and understandable representation of
% the system that can be communicated to non-technical users.
% 
% From a technical perspective, we believe that strong typing at every level is
% essential for building reliable software.  When implementing and analyzing
% transformation functions on the workflows, we would like the compiler to check
% that everything stays type-correct. By using a type system that is powerful
% enough to support features like GADTs, we can do typed transformations on the
% core datatypes.
% 
% Workflows are very much about the processes, i.e. the flow of the language.
% Another important part when modeling applications is the domain model. For
% rapid development and maintenance, it is important to keep the domain model
% flexible. This means that the domain-specific code should be kept to a minimum.
% Generic programming is a technique that can facilitate this. For example, using
% generic programming, we can automatically generate forms, overview pages,
% database interfaces and API interfaces. 
% 
% \subsection{Related work}
% 
% Orc \cite{orc} is a recent research language that is designed for asynchronous
% and concurrent applications, such as workflow modeling. Orc is an untyped
% language, and is run on the JVM. It is not meant as a general purpose
% language. We think that by embedding its functionality within a general purpose
% language like Haskell, it will be much easier to construct applications that use
% other libraries. Orc currently has no facility for generic programming.
% 
% iTasks \cite{iTasks} is a system built in Clean, a language quite similar to
% Haskell. It provided the main inspiration for this research proposal. However,
% iTasks is untyped at its very core. This makes it hard to reason about the
% correctness and reliability. Also, the sole product of a workflow description in
% iTasks is a web application. HTML is integrated in the process of describing
% workflows, which makes it very platform-specific.
% 
% There exist a number of commercial workflow modeling tools. Almost all of these
% provide just a visual interface for describing the workflows. We believe that a
% textual specification can be much more powerful, and that a visual graph should
% be the end-product of a workflow, not the way to design it.
% 
% \section{Research question}
% 
% This thesis project will investigate what is needed to have a workflow modeling tool
% where the implementor can build workflows in a composable way using the language
% Haskell. We want to know: 
% 
% \begin{itemize}
% \item How can we design a workflow modeling tool where a workflow specification is platform-agnostic?  
% \item How can we design that system in such a way that it has a type-safe core language?  
% \item How can we minimize application-specific code by using generic programming?
% \end{itemize}
% 
% \subsection{Contribution}
% 
% Compared to other approaches, we will deliver a solution that is strongly typed
% at the highest and lowest level. By using modern techniques like GADTs and
% type-level programming we can build libraries that are fully type-checked using
% the compiler. Also, we will build a number of libraries that can be used for
% building workflow applications.
% 
% \section{Approach}
% 
% We propose to build a workflow modeling tool in Haskell that provides the user
% with a way to express workflows in an implementation-agnostic way. We will port an
% existing workflow application that is written in PHP to show how our system
% compares against a manual implementation of a workflow application. 
% 
% Using this application as a starting point, we will try to keep the
% application-specific code to a minimum by generalizing as much code as possible
% into libraries. 
% 
% \subsection{The workflow library}
% \subsection{Generic Programming library}
% \subsection{Other libraries}
% 
% \section{Expected results}
% 
% We expect to end up with at least one application and two libraries. The first
% library will be for building interactive workflows. The other library will
% be for the generic programming on the domain model. It will provide facilities
% for writing your own generic function as well as a number of default functions.
% 
% We suspect that the ported application will have less than half the LOC compared
% to the original version. PHP provides not nearly as much mechanisms for
% abstraction as Haskell. Also, we will end up with a fully type-checked version
% of the application. Security is no longer an issue if we take the right
% precautions at the type-level. Also, we expect the application to run much
% faster than its PHP variant because we will compile it using an optimizing
% compiler, whereas the PHP variant is interpreted.

\section{Planning}

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}

-- implementation issues
