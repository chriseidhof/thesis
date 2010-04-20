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

We are not aware of a technique to encode data models in Haskell on the
conceptual level. However, many implementations exist to encode data models in
Haskell on the logical level.

The Happstack framework\footnote{\url{http://happstack.com}} contains a
component for constructing in-memory databases. It is
inspired by the Java Prevayler library \cite{wuestefeld2003you}, which works by
storing the entire database in memory and journalling changes on disk.
This approach leads to very fast code, but also leads to less portable code. If
the user wants to switch to a different type of database, it might involve a lot
of work.

The HaskellDB library \cite{bringert2004student, leijen2000domain} provides a
type-safe way to access relational database from Haskell. They allow the user to
construct queries and schemas that are valid, checked by the type system.

Outside of Haskell, the Core Data framework \cite{mark2009anatomy} provides a way to build data models
in Objective-C. In Core Data, a data model is designed graphically (on the
conceptual level), and then derives a data model on the logic level
automatically. It can provide interfaces to relational databases, flat files or
XML, and automatically provides undo support.
The interface for the programmer uses objects, and relationships are encoded as
pointers to other objects,  which provides a native feel for object-oriented
programmers.

\subsection{Continuations}

In Haskell, multiple libraries exist that provide a continuation-based interface
for web programming. However, none of them are actively maintained. In his paper
on arrows, \citet{hughes2000generalising} describes a library for web
programming with arrows. The WASH system \cite{thiemann2002wash,
thiemann2005embedded, thiemann2006wash} provides a library for building
page flows in a style similar to our library. However, their code as not been
actively maintained.

In the Clean programming language, the iTasks system \cite{plasmeijeridata,
plasmeijeriTasks} provides a monadic interface for building continuation-style
web applications. The Clean language provides native support for serializing
continuations. However, the Clean language is not as widely used as Haskell, and
therefore does not have the vast amount of packages available that exist for
Haskell.

The Seaside framework \cite{ducasse2007seaside} is an implementation of
continuation-based web programming in Smalltalk. Smalltalk is a highly dynamic
language that is not statically typed. Although Smalltalk programs are typically very
concise, they lack the static guarantees that a type-checker provides.

The Arc language\footnote{\url{http://arclanguage.org/}} is designed for web
programming, and also makes heavy use of continuations. As we have seen in
chapter \ref{chap:continuations}, their programs are very concise, although they
are arguably not as readable as our Haskell counterparts. Also, they are not
statically typed.

Finally, the LISP and Scheme communities have acted as pioneers in continuation-based web
programming.\cite{queinnec2000influence, restruct, graunke-programming, graunke-modeling,
krishnamurthi2007implementation, mccarthy2009restful, lispforwebbased}.
Although their research focuses on dynamically typed languages, they have
described a lot of useful program construction techniques as well as
automatic restructuring techniques.

\subsection{Generic Views}

The iData toolkit \cite{plasmeijeridata} is one of the first frameworks that uses generic programming
to generate views such as forms and HTML.
They also make use of view objects to customize generic programs, although it is
not clear whether they use similar techniques for building bidirectional
functions.

The WebObjects framework \cite{webobjects} also provides features for rapid
prototyping, named Direct2Web. In Direct2Web, the entire application is created
from the data model. Although Direct2Web is based on meta-programming features
such as reflection and introspection, it yields the same results. We believe
generic programming forms a more principled approach to meta-programming,
especially because everything is strongly typed and checked at compile-time.

\section{Future work}

In each of the sections we have discussed possible future work, which we will
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

Although we have built some small applications with the libraries, we think that
building larger examples will be of great benefit to the practical applications
of the libraries. We do not think that our libraries are finished yet, using
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

A different approach would be to build a domain-specific language that generates
Javascript. However, this might lead to a compromise in expressiveness, as it
will probably impossible to support arbitrary Haskell functions. Also, the
Haskell evaluation model is very different from the Javascript evaluation model, which
can lead to errors that are only detected at runtime.
This might be particularly true if laziness and infinite structures are used.

We think an AJAX library can be designed independently of the libraries built in
this thesis, but can be used in combination with these libraries.
We can envision a library that works in a functional reactive style, such as
other GUI toolkits \cite{carlsson1993fudgets, hudak2003arrows}. There is a
functional reactive programming framework in Javascript
\cite{meyerovich-flapjax} that is compiled using Haskell, perhaps it can be
integrated with Haskell.

However, compiling code to run in the web browser is only the first step: AJAX-style websites
also involve communication between the server and the client. The security is
important, for example, if we store the database password on the server, we have
to ensure it does not accidentally leak to the client. We think the research on
security analysis \cite{feedbackoriented, heintze1998slam,
pottier2003information} could be of great help here.

\section{Conclusion}

We have seen the development of an example application and the implementation of
three libraries that are used by that application. Together, they form a
framework that can be used to build web applications in a type-safe way. Generic
programming allows for rapid prototyping, but using lenses we can also change
generic programs.
We think that our work could form the basis of libraries that will be used in
real applications. 

%if not thesis

\bibliographystyle{plain}

\bibliography{bibliography}


\end{document}
%endif
