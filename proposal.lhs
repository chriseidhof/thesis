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
system to accomplish business-specific tasks. By modeling these workflows in
Haskell, a strongly typed functional language, we aim to provide powerful
building blocks to model these workflows and create working web applications. We
think that a lot of functionality is also useful for other web applications.

Using GADTs, we can develop a combinator-library similar to Clean's iTasks
library \cite{iTasks}, which does continuation-based web-programming
\cite{webInteractions}. Also, using a data-type generic programming library
inspired by EMGM \cite{EMGM} we can build generic functions similar to Clean's
iData \cite{iData}. For web applications, we can build a lot more generic
functions for dealing with XML, JSON, building APIs \cite{staticapis} and generating
documentation, for a start.

Our goal is to make it easy to write your own generic functions on the
data-model to achieve maximum reuse. By writing generic functions, we can change
the domain-model without having to worry about the code that generates the
forms, for example. This allows for real domain-driven design, and can prevent a
number of bugs: there is very little application-specific code.

In the domain model, there is often a lot of information that is vital for the
application but should not be presented to the user. Often, a user can only see
a projection (view) of the data and it is only possible to edit a subset of the
data. By using techniques similar to lenses \cite{lenses} we can provide
updatable views (or forms) like iEditors \cite{iEditors}.

Typically, web applications are built in a way that maps URLs to specific
actions. The workflow of web applications is very implicit: *goto alike*. TODO:
explain more about continuations.

\section{Comparison to other web programming languages and frameworks}

Iets over WASH \cite{wash}, WebFunctions \cite{webfunctions},
\cite{programmingtheweb}

\section{Building Continuations}

TODO: insert code example, explain.

\subsection{Using GADTs}
Tell something about why GADTs matter (also compared to clean)

\subsection{Analyzing the Task-datastructure}
\subsection{}

\section{Generic Programming}

Because our framework primarily targets web applications that are backed by a
database, we provide a lot of generic functions for database-like records. These
records are a subset of the regular Haskell datatypes: they have only one
constructor, that contains named fields.

TODO Introduce LEMGM.

\subsection{Forms}

TODO: Voorbeeld forms in php/ruby
Iets over formlets \cite{formlets}, curry and user interfaces \cite{curryui}.

\section{Bi-directional transformations}

Tell why this matters.

\section{Analyses}

Because our task-tree is a datastructure we can inspect, we can do analyses in a
type-safe way. An analysis can be seen as a transformation, for this we could
possibly use the 

\section{Conclusion}

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}
