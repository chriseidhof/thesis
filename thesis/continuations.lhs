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
\title{Continuation-based web programming}

\maketitle

%endif

\section{Introduction}
What is continuation-based web-programming?
  - how does normal web-programming work?
  - what is a continuation
  - related work (wash, iTasks, other languages)

\section{A monadic approach}
\label{sec:monadic}

As a first approach, we will build a monadic library.

%include continuations/Monadic.lhs

\section{An arrow-based approach}

%include continuations/ArrowBased.lhs

\section{Defunctionalization}

%include continuations/Defunctionalization.lhs

\section{Conclusion}

We have investigated a monadic approach, an arrow-based approach and
defunctionalization. The monadic approach has the simplest interface and is the
easiest to use. However, it lacks serialization of values. The arrow-based
interface is easier to serialize, but does not provide a nice interface.
Defunctionalization is a promising technique, but Haskell is unfortunately not
yet equipped with the right tools to do defunctionalization as a library or
using a preprocessor. For now, we will use the monadic library, because it has
the cleanest interface.

TODO: indexed monad with access control etc.

%if not thesis
\end{document}
%endif
