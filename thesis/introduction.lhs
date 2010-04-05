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

\section{Contributions}

This thesis contributes the 

\begin{itemize}
\item Library for ER-modeling in Haskell
\item Library for generic web programming
\item Library for continuations
\item Discussion of meta-programming in Haskell
\end{itemize}

Other contributions

\begin{itemize}
\item Released ER-modeling library
\item Wrote article that appeared on Hacker News
\item Submitted APLWACA paper: The future of the web is statically typed
\item Submitted ICFP paper:    Shifting focus with lenses in Haskell
\end{itemize}


Meta-programming problems:
\begin{itemize}
\item Complexity of type errors (ER-modeling)
\item Serialization of Haskell values
\end{itemize}

%if not thesis

\end{document}

%endif

