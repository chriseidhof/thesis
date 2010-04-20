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
forms an even better basis. We demonstrate this by building a web development framework.

Statically typed languages have numerous advantages over dynamic languages.
Type checking eliminates a large class of run-time errors and enables many
compile-time optimizations. Statically typed languages have the perceived
disadvantage of denoting all types explicitly, however: Haskell is a language that has type
inferencing, this disadvantage does not arise.

The framework we develop in this thesis is based on the 
\emph{model-view-controller} pattern \cite{krasner1988description}.
This leads to a separation of concerns and achieves good modularity:

\begin{itemize}
\item The model layer takes care of the data model and persistence.
\item The view layer displays model data to the end user.
\item The controller layer mediates between the model and view and contains application logic.
\end{itemize}

For each of these layers, we design a library. The libraries are completely
orthogonal but work very well together.
We present an example in chapter \ref{chap:quizexample} that describes how all
of the libraries designed in this thesis fit together.
This thesis addresses the following questions:

\begin{itemize}
\item What is a good abstraction for representing data models?
\item What is a good abstraction for representing interactions that span over multiple pages?
\item How can we use the type system to reduce boilerplace code as much as possible?
\end{itemize}

To answer these questions, we have looked at both programming language research
and web programming research.

In chapter \ref{chap:quizexample} we show a sample application 
using the libraries described in this thesis:
a system for creating and taking multiple-choice quizzes.

In chapter \ref{chap:ermodels} we describe how to represent and
work with ER models in Haskell.
ER models are a high-level abstraction of data models.
We implement a library to represent ER models and describe the
implementation of two backends: one is an in-memory
database, and the other an interface with a relational database.

Chapter \ref{chap:continuations} describes libraries for programming the web
using continuations to describe interactions.
These libraries form the controller part of our application.
Continuations add a stateful layer on top of the stateless HTTP
protocol. We have built multiple libraries, and discuss their differences.

In chapter \ref{chap:views} we describe how generic programming reduces
view boilerplate. We have built functions that generate view code based on
the structure of the input data, and we show how the generated code can be
customized in an elegant, functional way.

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

