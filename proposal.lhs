\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
%format :-> = "\mapsto"
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

% TODO: remove "often"
 
\maketitle

\abstract {
  During the course of this research we will try to make developing workflow
  systems in the language Haskell as easy as possible. By integrating several
  topics of research (generic programming, compiler techniques, domain specific
  languages) we hope to build an integrated framework composed of several
  orthogonal libraries.
}

\section {Introduction}

Most business processes these days are automated and made available online.
These processes typically contain complex interactions that enable the users of the
system to accomplish business-specific tasks.  A workflow systems describes
the interaction between humans and machines in these processes. Our goal is to
make developing these workflow systems as easy as possible. 

By expressing combinators for workflows in Haskell, a strongly typed functional
language, we aim to provide powerful building blocks to model these workflows
and create executable specifications. In contrast to related work in this area,
we strongly believe that the combination of both a typed approach and an
extensive set of readily available libraries can be a catalyst for building real
applications.

Using techniques like GADTs, we can develop a combinator-library similar to
Clean's iTasks library \cite{iTasks} or the standard combinators from Orc
\cite{orc}, which are both suited for continuation-based \cite{webInteractions}
web programming. We show how we can express all basic workflow patterns as
defined by Van Der Aalst \cite{workflowpatterns}.

Secondly, we would like to facilitate for domain driven design \cite{ddd} by
using a data-type generic programming library \cite{gpintro}. We can build
generic functions similar to Clean's iData \cite{iData}.  By writing generic
functions we can generate a lot of code from just our domain model.  This allows
for real domain-driven design, and can prevent a number of bugs: there is a
  minimal amount of application-specific code.

Thirdly, in the domain model, there is often a lot of information that is vital
for the application but should not be presented to the user. Often, a user can
only see a projection (view) of the data and it is only possible to edit a
subset of the data. Therefore, we want to explore techniques to facilitate
this.

In the last part of our research we focus on analyzing workflows. We want to be
able to transform and analyze the workflow graph. For example, we could draw a
diagram of the graph, optimize some flows or give information about the
complexity.


By combining all these ingredients into a single framework we hope to give a
powerful recipe to build your own web applications. We named our framework
Pesto, after the Italian sauce: the ingredients can be enjoyed on their own, but
it is the combination that gives true enjoyment.

\section{Workflow specifications}

% TODO: veel te low-level

A workflow specifies the way humans interact with a machine. However, there is
not always a clear mapping from a workflow specification to a working
application. The encoding of a workflow as an application is either done via
workflow modeling tools or manually. In our research we will try to
automatically generate web applications from a workflow specification. Our
approach is not limited to web applications, however. This technique is relevant
for construction of all kinds of user interfaces.

Typically, web applications are built in a way that maps URLs to standalone
programs, using HTTP. HTTP is a stateless protocol, which makes 
the workflow of web applications often very implicit when the programmer doesn't
use abstractions for this. In a lot of web applications, the state has to be
rebuilt from the ground up at every request.

By working with continuations (such as in WASH \cite{wash}) we can provide a different
interface for programming the web. Here, every requests loads a continuation
which was kept on the server. This allows to keep state without having to
explicitly restore it.

\subsection{Using GADTs}

By representing a workflow as a GADT we will have a deep embedding of our
combinator language, whereas if we would represent our workflows using only functions we
would have a shallow embedding. By using a deep embedding, we open up the way
for analysis and transformation. Using GADTs we can even enforce type-safety at
the datatype-level. If we encode our workflows in an Arrow-like style, we get
the following datatype:

> data (:->) a b where
>   Action      :: (Show a, Read a)  =>  (a -> Action b)          -> (a :-> b)
>   Edge        :: (Show b, Read b)  =>  (a :-> b)  -> (b :-> c)  -> (a :-> c)
>   Choice      ::    (a -> Bool)    ->  (a :-> c)  -> (a :-> c)  -> (a :-> c)

The |Action| constructor defines a single step that is taken. This action might
yield a value of type |b| or request an interaction with the user. An |Edge| is
a sequence of two steps, and |Choice| makes a dynamic choice based on a boolean
condition.  We have parameterized our datatype by both its input (|a|) and its
output (|b|).

We will investigate which basic constructs are necessary for expressing
and analyzing workflows.

\subsection{Building continuations}

So far, we have investigated two possible designs for our library. The first
design was based on monads. However, if we want to pause execution at some
point, and serialize it, we have to serialize the entire state, i.e. all free
variables. Consider the following example:

> register = do  name <- inputString
>                display "Are you sure you want to register?"
>                addUserToDatabase name

Here, the |name|-variable is used at a later point in the execution. However,
directly after executing the first line we don't know whether we are ever going
to need |name| again. Thus, we have to store it in our environment if we ever
want to use it again.

In contrast, when we work with arrows, we explicitly give the input and output
of each part of the computation. Therefore, if we want to store a continuation
we can suffice by storing a pair of type |(a, a :-> b)|. In our research so far,
we have also found the same solution in a paper by John Hughes about
generalizing Monads to Arrows \cite{Hughes98generalisingmonads}. It seems that
Arrows are a more natural fit for web programming in this style.

In our research, we will investigate which control abstractions are a good fit.
  
\section{Generic Programming}

Much of the functionality of an  application is directly dependent on the domain
model. In a lot of programming environments code has to be manually updated whenever
the domain model changes. However, in recent web frameworks a lot of
datatype-generic programming is done using meta-programming or code generation.
This helps keeping the domain-model flexible. However, it is notoriously hard to
write your own generic functions. Oftentimes, generated code is also edited by
the programmer, which makes it harder to change the original domain model.

Typically, web applications contain a fair amount of boilerplate code. We can
build generic functions to deal with this. For example, we can build
functions for dealing with XML, JSON, building APIs \cite{staticapis} and
generating documentation. However, the generic functions should not be limited
to those envisioned by the authors of a framework: it should be easy for the
users of the framework to write their own generic functions.

By choosing the right library, we wish to make it as easy as possible
for a developer using our framework to write her own generic function. By having
functionality as generic as possible you can easily change your domain model
without having to change a lot more code.

A sensible generic library might be \cite{emgm}, because we need a couple of its
key features. First, it should be possible for the user of the library to write
her own generic functions. Secondly, it should be possible for the user to add
her own datatypes.

We have built a variant of EMGM which we have used for prototyping our research.
We will investigate what the best library would be for doing generic programming
like this.

\subsection{Forms}

When writing a form for an HTML page, there are two parts of code that are
related: one part displays the form, the other parses the user input once the
form is submitted to the server. It is obvious that these parts are dependent on
each other, but often little care is taken to make sure they share the same
interface.

Therefore, we believe that the code for displaying and parsing a form should not
be written seperately. In the paper on formlets \cite{formlets}, Cooper et al.
design a library for generating these kinds of forms in a \emph{composable},
\emph{type-safe} way. Similar work has been done for user interfaces in the Curry language
\cite{curryui}. We will investigate different ways to build forms and discuss
their advantages and disadvantages.

\subsection{Bi-directional transformations}

When a user is confronted with the data from the domain model, it is often a
simplified view of the internal structure. This is because some properties of
the data model might be for internal use only or are only editable by a certain
group of users. In common web programming, these views are done in an ad-hoc
fashion. By using a technique similar to lenses \cite{lenses} or bi-directional
dependencies as described in a technical report by Schrage and Swierstra
\cite{haskelladl} we can provide these views and have strong guarantees about
the consistency of the transformations.

These views are very much like the embedding-projection pairs used in Generic
Programming, the main difference is that the data in the view is often
a subset of the original data, so there is not necessarily an isomorphism. We
will investigate the differences between the embedding-projection pairs and these
bidirectional views in order to see if we can abstract out common code and
patterns.

\section{Analyses}

Because our task-tree is a datastructure we can inspect, we can do analyses in a
type-safe way. An analysis can be seen as a transformation, for this we could
possibly build upon the work of Baars et al. \cite{tttas}. Some possible analyses
might be:

\begin{itemize}
\item Drawing a graph (e.g., a Petri-net) of the workflows.
\item Analyzing the complexity of workflows
\item Checking for deadlocks
\end{itemize}

\subsection{Observable Sharing}

We want to construct analyses on top of the basic workflows in such a way that
the system is extensible, i.e. it should be easy to add analyses. In order to do
these analyses, we need to observe sharing. For example, consider the following
workflow:

> addUsers = choices  [ ("Add another user",  userForm `Edge` addUsers)
>                     , ("Stop",              display "Thanks")
>                     ]

Here, the |addUsers| recursively calls itself. By using a general mechanism to
turn recursive structures into explicit graphs with explicit sharing we can
observe these recursive calls. This can also be used to compile our high-level
DSL into a lower-level (more optimized) representation. There is recent
unpublished work by Gill \cite{reify} that provides a way to turn recursive
structures into structures with explicit sharing. We can then use existing
compiler techniques to analyze our data structures.

\subsection{Finite State Machines}

Another way we could view our workflows is as finite state machines: an
|Action| is a state and the |Edge| and |Choice| constructors define edges and
choices, respectively. We will do literature research to investigate how
we can use existing theory and practice from finite state machine research to
our advantage.

\section{Related work}

In earlier days, Thiemann has built the Web Authoring System Haskell (WASH)
\cite{wash}. This is a set of libraries for doing web programming in Haskell.
However, it does not use generic programming nor focus on workflows. The library
has not been maintained for more than two years. The WASH system is also very
focused on lower-level building of HTML pages instead of describing workflows on
a high level.

We also have looked into WebFunctions \cite{webfunctions}. This is a
library that is designed for programming the web on a higher level. Again,
WebFunctions suffers from a lack of updates (the last update was in 2005).
WebFunctions doesn't use any generic programming, which makes it tedious to
write large programs where a lot of functionality can be automated.

One of the foundational papers in the field of web programming is by Felleisen
et al \cite{programmingtheweb}. They have shown how web programming can be
liberated from an imperative style by using continuations to represent web
applications.

Orc \cite{orc} is a recent research language that is designed for asynchronous
and concurrent applications. Orc is an untyped language, and is based on the
JVM. It is not meant as a general purpose language. We think that by embedding
its functionality within a general purpose language like Haskell, it will be
much easier to construct applications that use other libraries.

iTasks \cite{iTasks} is a system built in Clean, a language quite similar to
Haskell. It provided the main inspiration for this research proposal. However,
iTasks is built in a monadic style with a shallow embedding of the tasks, which
makes it currently impossible to analyze. Secondly, the environment of Clean is
quite isolated. By building on the foundations of Haskell we have a vast amount
of packages available that will save a lot of work for application programmers.

Lastly, recent work by Visser et al.  \cite{WebWorkFlow} describes the design
of WebWorkFlow, a language defined specifically for building web-based
workflows. By building a custom language they provide an integrated way to build
workflows. They use generic programming (the |derive| construct) to build forms,
but unfortunately, it is hard for users to write their own generic functions.
Also, like Orc, it is hard to integrate existing libraries into your code.

None of the libraries above make it easy to inspect the structure of the
generated application. This is something that is hard to later on add to a
language, and ideally is added while designing the (embedded) language. We
believe that this is a big advantage of our approach over all existing
approaches.

\section{Proven functionality}

To prove that this framework works for real applications we
will port an existing PHP-application to our system. Our goal is to
keep the code as small and readable as possible. Also, we expect that the
resulting application runs a lot faster than its PHP-variant because we
work in a compiled language, as opposed to an interpreted one.

\section{Research question}

Our main research question is: \emph{How can we model executable workflows in Haskell as
easy as possible?}.  We will try to answer that using the following subquestions:

\begin{itemize}
\item How can we use Haskell's type system to prevent errors?
\item How can we use generic programming to eliminate boilerplate?
\item Which analyses can we do on the workflow graph?
\end{itemize}

\section*{Planning}

We have split up our research into a couple of orthogonal topics that will be
investigated during week 27 - week 35. Week 36 and 37 are designated for the
integration of the research and porting the PHP application. Weeks 38-50 are
reserved for writing and 50-2 for preparing the presentation.
\\

\begin{tabular}{l l}
Week  & Action \\
26    & Proposal ready \\
27    & Research: Modeling continuations \\
28    & Research: Modeling continuations \\
29    & Research: Generic programming \\
30    & Research: Generic programming \\
31    & Research: Analyses \\
32    & Research: Analyses \\
33    & Research: Forms \& Bi-directional transformations \\
34    & Research: Finite State Machines \\
35-37 & Research: Porting an application \\
38-49 & Writing \\
50-2  & Presentation preparation \\
2     & Presentation
\end{tabular}

\nocite{arrowlets}
\nocite{iEditors}
\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}
