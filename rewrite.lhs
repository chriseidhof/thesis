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

\section{Introduction}
 
\maketitle
Virtually every business is automated these days, in varying degrees. For
maximal efficiency, some business processes are automated. In the system that
comes out of such an automation there is a specification of which steps need to
be taken and how humans interact with the computer. We call such a system a
\emph{workflow application}.

There exist a number of systems designed to make modeling of these business
processes easier. They aim to have executable specifications of the
different workflows in an organization. We call such a system a \emph{workflow
modeling tool}. The person or team who implements such a workflow is called the
\emph{implementor}

One of the most sophisticated workflow modeling tools is iTasks.
In iTasks, the programmer gives a specification of the workflow in the programming
language Clean. That description is directly executable: it can be run as a web
application. Because the workflow is written down as a Clean program, the
designer has all the advantages of a modern higher-order language.

We believe that having an executable specification is essential. However, it
should not be limited to just one platform. A platform like iTasks is focused
solely on web applications. Web applications are just one way to present a
workflow system. We would like to have a system where a workflow is an abstract
description of a business process, and not mixed with implementation-specific
code such as HTML fragments. For example, as mobile phones get more advanced, it
would make a lot of sense to have a mobile version of a workflow application
next to a web application.

When designing an application that contains the workflow of an organization,
there are multiple stakeholders. Not all of these people might know how to read
a specification in the form of a computer program. On the other side, the
implementor might have to deal with unclear or incomplete requirements. From our
own experience, we know that a lot of misunderstanding can be helped by
integrating all stakeholders in the process of developing an application. In a
workflow system, workflows can be modelled as graphs and thus have a direct
visual representation. By drawing this graph automatically from the
specification there is always an up-to-date and understandable representation of
the system.

From a technical perspective, we believe that strong typing is essential for
building reliable software. While the combinators of iTasks have rich types,
iTasks is untyped at its core. When implementing and analyzing transformation
functions on the core syntax, we would like the compiler to check that
everything stays type-correct. By using a type system that is powerful enough to
support features like GADTs, we can do typed transformations on the core datatypes.

Workflows are very much about the processes, i.e. the flow of the language.
Another important part when modeling business processes is the domain model. For
rapid development and maintenance, it is important to keep the domain model
flexible. This means that the domain-specific code should be kept to a minimum.
Generic programming is a technique that can facilitate this. For example, using
generic programming, we can automatically generate forms, overview pages,
database interfaces and API interfaces. We could not possibly envision all the
generic functions, so a requirement is that it should be easy for a programmer
to write their own generic functions.

TODO: literature overview.

\section{Research question}

\subsection{Contribution}

Compared to other approaches, we will deliver a solution that is strongly typed
at the highest and lowest level. By using modern techniques like GADTs and
type-level programming we can build libraries that are fully type-checked using
the compiler.

\section{Approach}

We propose to build a workflow modeling tool in Haskell that provides the user
with a way to express workflows in an implementation-agnostic way. We port an
existing workflow application that is written in PHP to show how our system
compares against a manual implementation of a workflow application. We will
discuss how this application could have been implemented using other workflow
modeling tools.

Using this application as a starting point, we will try to keep the
application-specific code to a minimum by generalizing as much code as possible
into libraries. 

\section{Expected results}

We expect to end up with at least one application and two libraries. The first
library will be for building interactive workflows. The other library will
be for the generic programming on the domain model. It will provide facilities
for writing your own generic function as well as a number of default functions.

We suspect that the ported application will have less than half the LOC compared
to the original version. PHP provides not nearly as much mechanisms for
abstraction as Haskell. Also, we will end up with a fully type-checked version
of the application. Security is no longer an issue if we take the right
precautions at the type-level. Also, we expect the application to run much
faster than its PHP variant because we will compile it using an optimizing
compiler, whereas the PHP variant is interpreted.

\section{Planning}
\section{Bibliography}

\end{document}

-- implementation issues
