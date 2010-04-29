%if not thesis

\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
%include forall.fmt 
%include chris.fmt 
% vim:spell
\usepackage{a4wide}
\usepackage{natbib}
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
\title{Conclusion}

\maketitle

\tableofcontents

%endif

\section{Related work}

\subsection{Data models}

We are not aware of a technique to encode data models in Haskell at the
conceptual level. However, many implementations exist to encode data models in
Haskell at the logical level.

The Happstack framework\cite{happstack} contains a
component for constructing in-memory databases. It is
inspired by the Java Prevayler library \cite{wuestefeld2003you}, which works by
storing the entire database in memory and journalling changes on disk.
This approach leads to very fast response times, but also leads to less portable code. If
the user wants to switch to a different type of database, it might involve a lot
of work.

The HaskellDB library \cite{bringert2004student, leijen2000domain} provides a
type-safe way to access relational databases from Haskell. It allows the user to
construct queries and schemas that are valid and checked by the type system.

Outside of Haskell, the Core Data framework \cite{mark2009anatomy} provides a way to build data models
in Objective-C. In Core Data, a data model is designed graphically (on the
conceptual level), and Core Data derives a data model on the logic level
automatically. It has interfaces to relational databases, flat files or
XML, and automatically provides undo support for data models.
The interface for the programmer uses objects, and relationships are encoded as
pointers to other objects, thus providing a native feel to object-oriented
programmers.

\subsection{Continuations}

In Haskell, multiple libraries exist that provide a continuation-based interface
for web programming. However, none of them is actively maintained. In his paper
on arrows, Hughes \cite{hughes2000generalising} describes a library for web
programming with arrows. The WASH system \cite{thiemann2002wash,
thiemann2005embedded, thiemann2006wash} provides a library for building
page flows in a style similar to our library.

In the Clean programming language, the iTasks system \cite{plasmeijeridata,
plasmeijeriTasks} provides a monadic interface for building continuation-style
web applications. The Clean language provides native support for serializing
continuations. However, the Clean language is not as widely used as Haskell, and
there is not such a vast number of packages available compared to Haskell.

The Seaside framework \cite{ducasse2007seaside} is an implementation of
continuation-based web programming in Smalltalk. Smalltalk is a dynamically typed 
language. Although Smalltalk programs are typically
concise, they lack the static guarantees that a type-checker provides.

The Arc language \cite{arclang} is designed for web
programming, and also makes heavy use of continuations. As we have seen in
chapter \ref{chap:continuations}, Arc programs are very concise, although 
arguably not as readable as our Haskell counterparts. Also, they are not
type-checked.

Finally, the LISP and Scheme communities have acted as pioneers in continuation-based web
programming.\cite{queinnec2000influence, restruct, graunke-programming, graunke-modeling,
krishnamurthi2007implementation, mccarthy2009restful, lispforwebbased}.
Although their research focuses on dynamically typed languages, they have
described a lot of useful program construction techniques as well as
automatic restructuring techniques.

\subsection{Generic Views}

The iData toolkit \cite{plasmeijeridata} is one of the first frameworks that uses generic programming
to generate views such as forms and HTML.
iData also makes use of view objects to customize generic programs, although it is
not clear whether similar techniques are used for building bidirectional
functions.

The WebObjects framework \cite{webobjects} also provides features for rapid
prototyping, named Direct2Web. In Direct2Web, the entire application is created
from the data model. Although Direct2Web is based on meta-programming features
such as reflection and introspection, it provides features that are similar to our generic functions.
Generic programming forms a more principled approach to meta-programming,
because generic programs are type-checked.

\section{Future work}

In each of the sections we have discussed possible future work, which we 
summarize here. For ER models,
we can extend the library to support full ER models with relationships between more
than two entities, attributes on relationships, add support for primary keys and
add more backends.

The continuations library is not finished yet. The arrow-based library could
greatly benefit from support for serialization.
Also, we have only built one application with it.
We think that building multiple, different applications will lead to a versatile
library.

The generic programming library for building views is motivated by our own
examples. In the future, we can add support for more kinds of generic views for
different datatypes.

Although we have built some small applications with the libraries, 
building larger examples will be of great benefit to the practical applications
of the libraries. We do not think that our libraries are finished yet, but using
them for real applications will give a good indication of what features are
lacking.

\subsection{AJAX}

One library that is lacking in Haskell is a good way to program interactive widgets in an
AJAX \cite{garrett2005ajax} style. AJAX involves code that is run on the client: inside the
client's web browser.
Some frameworks, such as Links \cite{cooper2006links}, do this by compiling their language to
Javascript \cite{ecma262}, which is the only widely supported programming language for
client-side scripting. In Haskell, this could be achieved using the YHC Compiler
\cite{golubovsky2007yhc} that supports compilation to Javascript.

A different approach is be to build a domain-specific language that generates
Javascript. However, this may lead to a compromise in expressiveness, as it
is probably impossible to support arbitrary Haskell functions. Also, the
Haskell evaluation model is very different from the Javascript evaluation model, which
may easily lead to errors that are only detected at runtime;
this is particularly true if laziness and infinite values are used.

We think an AJAX library can be designed independently of the libraries built in
this thesis, but can be used in combination with these libraries.
We can envision a library that works in a functional reactive style, such as
other GUI toolkits \cite{carlsson1993fudgets, hudak2003arrows}. There is a
functional reactive programming framework in Javascript
\cite{meyerovich-flapjax} that is compiled using Haskell: perhaps it can be
integrated with Haskell.

However, compiling code to run in the web browser is only the first step: AJAX-style websites
also involve communication between the server and the client. Here, security is
important, for example: if we store the database password on the server, we have
to ensure it does not accidentally leak to the client. Research on
security analysis \cite{feedbackoriented, heintze1998slam,
pottier2003information} could be of great help here.

\section{Conclusion}

We have seen the development of an example application and the implementation of
three libraries that are used by that application. Together, they form a
framework that can be used to build web applications in a type-safe way. Generic
programming allows for rapid prototyping, but using lenses we can also change
generic programs.
We think that our work may form a good starting point for libraries to be used in
real applications. 

%if not thesis

\bibliographystyle{plain}

\bibliography{bibliography}


\end{document}
%endif
