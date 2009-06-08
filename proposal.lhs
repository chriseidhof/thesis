\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}

\begin{document}
\author{Chris Eidhof}
\title{Thesis Proposal: Generic Web Interaction in Haskell}

% TODO: remove "often"
 
\maketitle

\section {Introduction}

Most business processes these days are automated and made available online.
These processes typically contain complex interactions that enable the users of the
system to accomplish business-specific tasks.  A workflow systems describes
the interaction between humans and machines in these processes.

By expressing combinators for workflows in Haskell, a strongly typed functional
language, we aim to provide powerful building blocks to model these workflows
and create executable specifications.

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
the datatype-level.

\subsection{Building continuations}

There are several approaches to representing continuations. We could represent
them with a monadic interface or an arrow interface.  Each of them has its
advantages. However, as Hughes described in Generalizing Monads to Arrows
\cite{Hughes98generalisingmonads}, monads are not optimal for representing these
types of continuations. Due to the nature of monads, we can not easily serialize
a monadic expression. Serialization is of vital importance if we want to be able
to stop and resume our application. 
  
We will investigate whether Arrows are the best fit for representing these
continuations. So far, it looks promising. One of the other applications of
arrows is GUI applications. For example, there is the Arrowlets \cite{arrowlets}
library that aims to make browser-based GUI programming (in which there also are
asynchronous events) easier.

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

\subsection{Forms}

When writing a form for an HTML page, there are two parts of code that are
related: one part displays the form, the other parses the user input once the
form is submitted to the server. It is obvious that these parts are dependent on
each other, but often little care is taken to make sure they share the same
interface.

Therefore, we believe that the code for displaying and parsing a form should not
be written seperately. In the paper on formlets \cite{formlets}, Cooper et al.
design a library for generating these kinds of forms in a composable, type-safe
way. Similar work has been done for user interfaces in the Curry language
\cite{curryui}.

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
investigate the differences between the embedding-projection pairs and these
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

We want to construct analyses on top of the basic workflows in such a way that
the system is extensible, i.e. it should be easy to add analyses.

Here, it wil be
interesting to see how we can deal with loops.

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

\section*{Planning}

% TODO: Compleet werkplan

May 30: reading, proposal ready.
June-Aug: research (implementing, reading)
Sep-Dec: writing

\section*{TODO}
\cite{emgm}
\cite{iEditors}

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}
