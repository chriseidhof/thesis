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
 
\maketitle

\section {Introduction}

A lot of business systems these days are automated and made available online.
These systems typically contain complex workflows that enable the users of the
system to accomplish business-specific tasks. By expressing combinators for
workflows in Haskell, a strongly typed functional language, we aim to provide
powerful building blocks to model these workflows and create executable
specifications.

Using techniques like GADTs, we can develop a combinator-library similar to
Clean's iTasks library \cite{iTasks} or Orc's standard combinators, which 
are both suited for continuation-based \cite{webInteractions} programming. We
will show how we can express all basic workflow patterns as defined by Van Der
Aalst (TODO).

Also, using a data-type generic
programming library inspired by EMGM \cite{EMGM} we can build generic functions
similar to Clean's iData \cite{iData}. 
By using generic functions, we can change the domain-model
without having to change the code that generates the forms, for example.
This allows for real domain-driven design, and can prevent a number of bugs:
there is a minimal amount of application-specific code.

In the domain model, there is often a lot of information that is vital for the
application but should not be presented to the user. Often, a user can only see
a projection (view) of the data and it is only possible to edit a subset of the
data. Therefore, we want to explore techniques similar to lenses \cite{lenses}
we can provide updatable views.

The last part of our system will focus on analyzing workflows. Because we have
deep embedding of our combinator language, we can not only execute workflows but
also transform and analyze the workflow graph. Here, it wil be interesting to
see how we can deal with loops.

\section{Workflow specifications}

Typically, web applications are built in a way that maps URLs to specific
actions. The workflow of web applications is very implicit: it is much like
goto. For every page there is one or more URLs that can be reached from it, and
often this is encoded in the view where it should be part of the domain logic.

By working with continuations (cite WASH, PG) we can provide a different
interface for programming the web. Here, every requests loads a continuation (a
function with an environment) which was stored on the server. This allows you as
a programmer to keep state without having to explicitly restore it.

We represent our web-application by a number of Tasks. We will investigate what
the minimal number of basic combinators is to be able to have a language that is
powerful enough to model all common web-applications. We will then build
combinators on top of that. Higher-order combinators are also interesting, but
might be problematic if we want to do analyses.

TODO: WebWorkFlows \cite{WebWorkFlow}

\subsection{Using GADTs}
Tell something about why GADTs matter (also compared to clean)

\section{Generic Programming}

For web applications in general, we can
build a lot more generic functions for dealing with XML, JSON, building APIs
\cite{staticapis} and generating documentation, for a start.
Our goal is to make it easy to write your own extensible generic functions on
the data-model. 

Often much of the application functionality is directly dependent on the domain
model. In a lot of programming environments much code has to be changed whenever
the domain model changes. However, in recent web frameworks a lot of work is
done using meta-programming or code generation. This helps keeping the
domain-model flexible. However, it is notoriously hard to write your own generic
functions, and that is one of the problems we want to solve by having a
structured approach to generic programming.

By developing a library similar to EMGM, we wish to make it as easy as possible
for a developers using our framework to her own generic function. By having
functionality as generic as possible you can easily change your domain model
without having to change a lot more code.

However, there is an issue when working with generic functions: often, you would
want a function like the generic one but with just a small exception. This can
be captured by using |newtype|s. For example, the default form element for an
integer-value is a textbox that tries to parse the 

TODO: write about doing this in views (or even forms) only

TODO: tell about specialization/customization of generic functions using, e.g.,
newtype.

\subsection{Forms}

TODO: Voorbeeld forms in php/ruby
Iets over formlets \cite{formlets}, curry and user interfaces \cite{curryui}.

\subsection{Bi-directional transformations}

 (or forms) like iEditors \cite{iEditors}.

When a user is confronted with the data from the model, it is often a simplified
view. This is because some properties of the data model might be for internal
use only or are only editable by a certain group of users. Often, these views
are done in an ad-hoc fashion. By using a technique similar to lenses
\cite{lenses} or bi-directional dependencies as described in a technical report
by Schrage and Swierstra \cite{haskelladl} we can provide these views and have
reasonable guarantees about the consistency of the transformations.

These views are very much like the embedding-projection pairs used in EMGM, the
main difference is that the view of the original data is often a subset, so
there is not necessarily an isomorphism. We will investigate the differences
between the embedding-projection pairs and these bidirectional views in order to
see if we can abstract out common code and patterns.

\section{Analyses}

Because our task-tree is a datastructure we can inspect, we can do analyses in a
type-safe way. An analysis can be seen as a transformation, for this we could
possibly build upon the work of Swierstra \cite{ttaas}. Some possible analyses
might be:

\begin{itemize}
\item Drawing a graph (e.g., a Petri-net) of the workflows.
\item Analyzing the complexity of workflows
\item Checking for deadlocks
\end{itemize}

We want to construct analyses on top of the basic workflows in such a way that
the system is extensible, i.e. it should be easy to add analyses.

\section{Related work}

Iets over WASH \cite{wash}, WebFunctions \cite{webfunctions},
\cite{programmingtheweb}, Orc (TODO)

\section{Conclusion}

A lot of the techniques described above have been implemented partially and
often without integrating them into one system. Our goal is to reuse (and
reimplement where necessary) these techniques into one system for writing web
applications. To prevent ourselves from writing unnecessary functions we will
port an existing PHP-application to our system. Our goal is to
keep the code as small and readable as possible. Also, we expect that the
resulting application will run a lot faster that its PHP-variant because we
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
problem will give rise to new solutions and insights. Lastly, the other two
systems are currently mainly used for academic purposes. By porting a real
application we want to show that our system works on more than just toy
examples.

\section*{Planning}

May 30: reading, proposal ready.
June-Aug: research (implementing, reading)
Sep-Dec: writing

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}
