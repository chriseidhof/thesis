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
\subsection{Serialization}
When we run our program as a process, we can keep the continuations in-memory.
However, web applications are sometimes forced to run in CGI mode, which means
that the entire program is run every request, and all state has to be saved
to disk. Also, it could be the case that the web application is run on a number
of servers, which typically means memory is not shared. However, continuations
should be shared across all the machines, for which serialization is necessary.

\section{A monadic approach}

As a first approach, we will build a monadic library.

%include continuations/Monadic.lhs

\section{An arrow-based approach}

%include continuations/ArrowBased.lhs

\section{Building an AST}

%include continuations/AST.lhs

\section{Conclusion}
Extension: role-based actions
The conclusion

%if not thesis
\end{document}
%endif
