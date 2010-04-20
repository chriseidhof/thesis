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

%include formatting.lhs

\begin{document}
\author{Chris Eidhof}
\title{Introduction}

\maketitle

\tableofcontents

%endif

\section{Introduction}

Web programming is a field that has been dominated by dynamic languages, because
of their perceived expressivity, meta-programming abilities and flexibility. In
this thesis, we show that the statically typed programming language Haskell
forms a good basis to build a web development framework.

Statically typed languages have numerous advantages over dynamic languages.
Type checking eliminates a large class of run-time errors and generally leads to
code that can be optimized heavily. Haskell is a language that has type
inferencing, removing the burden of annotating every variable explicitly. We
see how we can leverage these properties to make web programming in Haskell
a good alternative to dynamically typed languages.

In this thesis, we build a web framework in Haskell based on the 
\emph{model-view-controller} pattern \cite{krasner1988description}. We believe
that this leads to a separation of concerns and achieves good modularity:

\begin{itemize}
\item The model layer takes care of the data storage.
\item The view layer displays model data to the application user.
\item The controller layer coordinates action between the model and view.
\end{itemize}

For each of these layers, we have found abstractions.
Therefore, we try to design three orthogonal libraries, one for each layer.
We also show a larger example in chapter \ref{chap:quizexample} that describes how all
of the libraries designed in this thesis fit together.
This thesis tries to answer the following questions:

\begin{itemize}
\item What is a good abstraction for representing data models?
\item What is a good abstraction for representing interactions that span over multiple pages?
\item How can we generate as much view code as possible, thus reducing the amount of boilerplate?
\end{itemize}

To answer these questions, we have looked at both programming language research
and web programming research. We try to answer each question by
implementing a library.

In chapter \ref{chap:quizexample} we show a sample application that we have built
using the libraries described in this thesis.
It describes a system for creating and taking multiple-choice quizzes, and
describes the data model, the view and the controller.

In chapter \ref{chap:ermodels} we describe a library for representing and
working with ER models in Haskell.
We believe they form a high-level abstraction of data models.
We describe a library to represent ER models and describe the
implementation of two different backends: one backend builds an in-memory
database from the ER model, and the other backend builds an interface with a
relational database.

Chapter \ref{chap:continuations} describes libraries for programming the web
with continuations. They form the controller part of our application.
We believe continuations are a good abstraction of interactions
between multiple web pages: it adds a stateful layer on top of the stateless HTTP
framework. We have built multiple libraries, and discuss their differences.

Chapter \ref{chap:views} describes how we have used generic programming to
reduce view boilerplate. We have built functions that generate view code based on
the structure of the input data, and show how to customize the generated code
in an elegant, functional way.

In chapter \ref{chap:conclusion}, we describe related and future work and conclude.

\section{Contributions}

This thesis describes a web programming framework in Haskell that consists of
the following libraries:

\begin{itemize}
\item A library for ER-modeling in Haskell
\item A library for generic web programming
\item A library for continuations
\end{itemize}

Furthermore, we show an integrated example that uses all three libraries. To
implement these libraries, we make heavy use of type-level programming, generic
programming and bidirectional progamming.
We also introduce a new program construction technique: the use of bidirectional
programming to change generic programs.

This thesis work formed the basis for two papers. The first one is submitted to
the APLWACA conference and is aimed at web programmers that do not use
statically typed programming languages.
The second paper discusses the design and implementation of a bi-directional
programming library.
The second paper contains examples of combining bi-directional programming with
generic programming that stem from the problems we encountered when working on
the research done in this thesis.
Finally, we have written about one of the continuation-based web programming
libraries and submitted the post to Hacker
News\footnote{\url{http://news.ycombinator.com/item?id=1004701}}, which resulted
in a large discussion.

%if not thesis

\end{document}

%endif

