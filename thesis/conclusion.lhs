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

\subsection{ER models}
- happstack
- core data
\subsection{Continuations}
- Wash
- Arrows
- iTasks
Smalltalk
- Seaside
Paul Graham
Lisp (felleisen)
\subsection{Generic Views}
- iData \cite{plasmeijeridata}
- Ruby on Rails
\subsection{Frameworks}

links

\section{Future work}

In each of the sections we have discussed possible future work. For ER models,
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

\subsection{AJAX}

One library that is lacking in Haskell is a good way to program interactive widgets in an
AJAX \cite{garrett2005ajax} style. AJAX involves code that is run client-side: inside the
clients's web browser. Some frameworks, such as Links \cite{cooper2006links}, do this by compiling their language to
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
We can envision a library that works in a functional reactive style
\cite{carlsson1993fudgets, hudak2003arrows, meyerovich-flapjax}.

\section{Conclusion}

Reflection

Contributions

\section{Acknowledgements}

First of all, I would like to thank Doaitse for supervising my work. His
comments have been very valuable, and I have learned a lot from him.
It has always been a pleasure to discuss my thesis work, the future of
functional programming and all other things.
It was an honour to have Doaitse as my thesis supervisor.

I am also very thankful to Andres for being my second supervisor.
He is always a source of inspiration and knowledge, and has been very helpful.


Eelco has been a great friend and business partner in the last few years.
He showed me how to work very hard and have fun at the same time.
Although we now will both go our own ways, I have had lots of fun and learned a lot from him.

I would like to thank the software technology staff and students for providing
a great environment to study in.
In particular, I would like to thank Sebastiaan, Tom and Erik for the times we
have worked together and all the interesting discussions we have had.

I would like to thank my parents for supporting me. They have always believed in
me, even though I can not explain to them what it is that I am doing.

Finally, I would like to thank Janneke for her love and support. 

%if not thesis

\bibliographystyle{plain}

\bibliography{bibliography}


\end{document}
%endif
