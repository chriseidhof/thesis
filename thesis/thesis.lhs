\documentclass[11pt, twoside, a4paper, openright]{report}

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
%let thesis = True

\begin{document}
\author{Chris Eidhof}
\title{Describing ER models in Haskell}

\maketitle
\setcounter{tocdepth}{1}
\tableofcontents

\section*{Changelog}
\begin{itemize}
\change{january 12}{Reading/writing about defunctionalization}
\change{january 7}{Researched monadic continuations with Ref}
\change{january 5}{Wrote more about arrow-based continuations}
\change{december 17-18}{Started chapter about continuations}
\change{december 16}{Edited page 1-10, mostly inconsistencies}
\change{december 15}{Started on section about queries: \ref{sec:query}}
\end{itemize}

\chapter{Introduction}
\chapter{Entity Relationship Models}
%include ermodels.lhs
\chapter{Continuation-based web programming}
%include continuations.lhs
\chapter{Generic views}
%include views.lhs

\chapter{An example application}

Show an application that uses the ER-model library for the data model, the continuation-based library for the controller and the views library for the views.

\chapter{Conclusion}

\section{Writing DSLs in Haskell}
- write about the deficiencies: binding, type-level programming, observable sharing, etc.

\section{Web-programming in Haskell}

\end{document}
