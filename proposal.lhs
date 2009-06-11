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
\title{Thesis Proposal: Generic Web Interactions in Haskell}

% TODO: remove "often"
 
\maketitle

\abstract {
  In this research we will try to make developing workflow systems in the
  language Haskell as naturally as possible. By integrating several topics of
  research (Generic Programming, Compiler techniques
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
we strongly believe that a typed approach and an extensive set of readily
available libraries are vital for the succes of building real applications.

Using techniques like GADTs, we can develop a combinator-library similar to
Clean's iTasks library \cite{iTasks} or the standard combinators from Orc
\cite{orc}, which are both suited for continuation-based \cite{webInteractions}
programming. We show how we can express all basic workflow patterns as
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

\section{Workflow specifications}

% TODO: veel te low-level

A workflow specifies the way humans interact with a machine. However, there is
not always a clear mapping from a workflow specification to a working
application. The encoding of a workflow as an application is either done via
workflow modeling tools or manually. In our research we will try to
automatically generate web applications from a workflow specification. Our
approach is not limited to webapplications, however. This technique is relevant
for construction of all kinds of user interfaces.

Typically, web applications are built in a way that maps URLs to standalone
programs.  The workflow of web applications is very implicit: every program may
send the user to a different program on the webserver. There are often no real
contracts between the input/output of those programs, and no state is shared.
This also makes web applications notoriously hard to compose.

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
yield a value or request an interaction with the user. An |Edge| is a sequencing
of two steps, and |Choice| makes a dynamic choice based on a boolean condition.
We have parameterized our datatype by both its input (|a|) and its output (|b|).

We will investigate which basic constructs are neccessary for expressing
and analyzing workflows.

\subsection{Building continuations}

So far, we have investigated two possible designs for our library. The first
design was based on monads. However, if we want to pause execution at some
point, and serialize it, we have to serialize the entire state, i.e. all free
variables. Consider the following example:

> register = do name <- inputString
>               display "Are you sure you want to register?"
>               addUserToDatabase name

Here, the |name|-variable is used at a later point in the execution. However,
directly after executing the first line we don't know whether we are ever going
to need |name| again. Thus, we have to store it in our environment if we ever
want to use it again.

In contrast, when we work with arrows, we explicitly give the input and output
of each part of the computation. Therefore, if we want to store a continuation
we can suffice with a pair of type |(a, a :-> b)|. In our research so far, we
have also found the same solution in a paper by John Hughes about generalizing
Monads to Arrows \cite{Hughes98generalisingmonads}. It seems that Arrows are a
more natural fit for web programming in this style.

We will investigate which abstractions are a good fit.
  
\section{Generic Programming}

Typically, web applications contain a fair amount of boilerplate code. We can
build generic functions to deal with this. For example, we can build
functions for dealing with XML, JSON, building APIs \cite{staticapis} and
generating documentation. However, it should be easy for the users of the
library to write their own generic functions.

Often much of the application functionality is directly dependent on the domain
model. In a lot of programming environments much code has to be changed whenever
the domain model changes. However, in recent web frameworks a lot of
datatype-generic programming  is done using meta-programming or code generation.
This helps keeping the domain-model flexible. However, it is notoriously hard to
write your own generic functions. Oftentimes, generated code is also edited by
the programmer, which makes it harder to change the original domain model.

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

Here, the |addUsers| recursively calls itself. By using a general mechanism to turn
recursive structures into explicit graphs with explicit sharing we can observe
these recursive calls. This can also be used to compile our high-level DSL into
a lower-level (more optimized) representation. There is recent unpublished work
by Gill \cite{reify} that provides a way to turn recursive structures into
structures with explicit sharing. We can then use existing compiler techniques
to analyze our data structures.

\subsection{Finite State Machines}

Another way we could view our workflows is as finite state machines: an
|Action| is a state and the |Edge| and |Choice| constructors define edges and
choices, respectively. Here, we will do literature research to investigate how
we can use existing theory and practice from finite state machine research to
our advantage.

\section{Related work}

Iets over WASH \cite{wash}, WebFunctions \cite{webfunctions},
\cite{programmingtheweb}, Orc (TODO)
TODO: WebWorkFlows \cite{WebWorkFlow}

\section{Conclusion}

% TODO: helder geformuleerde vraag

A lot of the techniques described above have been implemented partially and
often without integrating them into one system. Our goal is to reuse (and
reimplement where necessary) these techniques into one system for writing web
applications. To prevent ourselves from writing unnecessary functions we 
port an existing PHP-application to our system. Our goal is to
keep the code as small and readable as possible. Also, we expect that the
resulting application runs a lot faster that its PHP-variant because we
work in a compiled language, as opposed to an interpreted one.

Of course, with iTasks and Orc available, why would there be a need for another
system? First of all, we believe strong typing is very important if you want to
write robust applications. Orc is currently not typed yet, and iTasks uses the
|Dynamic| type a lot because there is no support yet for type constructs such as
GADTs. Secondly, using Haskell we have the benefit of having a huge community
that has written a lot of software which we can build upon. In Orc it is very
easy to use Java classes and use other Java libraries, but, as the authors state
in their User Guide, it is not meant to be a general purpose language. Thirdly,
we believe that exploring alternative implementations and solutions of the same
problem gives rise to new solutions and insights. Lastly, the other two
systems are currently mainly used for academic purposes. By porting a real
application we want to show that our system works on more than just toy
examples.

\subsection{Research question}

Our main research question is: \emph{How can we model workflows in Haskell as
naturally as possible?}.  We will try to answer that using the following subquestions:

\begin{itemize}
\item How can we use Haskell's type system to prevent errors?
\item How can we use generic programming to eliminate boilerplate?
\item Which techiques for bidirectional transformations can we use to 
\item Which analyses can we do on the workflow graph?
\end{itemize}

\section*{Planning}

We have split up our research in a couple of orthogonal topics that will be
investigated during week 27 - week 35. Week 36 and 37 are designated for the
integration of the research in the weeks before. Weeks 38-50 are reserved for
writing and 50-2 for preparing the presentation.

\begin{tabular}{l l}
Week  & Action \\
26    & Proposal ready \\
27    & Research: Modeling continuations \\
28    & Research: Modeling continuations \\
29    & Research: Generic programming \\
30    & Research: Generic programming \\
31    & Research: Forms \\
32    & Research: Bi-directional transformations \\
33    & Research: Analyses \\
34    & Research: Analyses \\
35    & Research: Finite State Machines \\
36    & Research: Integration \\
37    & Research: Integration \\
38-49 & Writing \\
50-2  & Presentation preparation \\
2     & Presentation
\end{tabular}

\nocite{arrowlets}
\nocite{iEditors}
\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}
