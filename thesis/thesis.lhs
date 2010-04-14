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
\usepackage{natbib}
\usepackage{todonotes}

%include formatting.lhs
%let thesis = True

\begin{document}
\author{Chris Eidhof}
\title{Web programming in Haskell}

\maketitle
\setcounter{tocdepth}{1}
\tableofcontents

\section*{Changelog}
\begin{itemize}
\change{april 13}{Added section about Quiz example}
\change{april 13}{First draft about views ready for review}
\change{april 9}{First draft about continuations ready for review}
\change{april 6}{Wrote section about er-models}
\change{march 16}{wrote library documentation for Basil (ER-modeling)}
\change{march 10}{Wrote about formlets in view part}
\change{march 3}{made planning}
\change{january 12}{Reading/writing about defunctionalization}
\change{january 7}{Researched monadic continuations with Ref}
\change{january 5}{Wrote more about arrow-based continuations}
\change{december 17-18}{Started chapter about continuations}
\change{december 16}{Edited page 1-10, mostly inconsistencies}
\change{december 15}{Started on section about queries: \ref{sec:query}}
\end{itemize}

\chapter{Introduction}
%include introduction.lhs
\chapter{Entity Relationship Models}
\label{chap:ermodels}
%include ermodels.lhs
\chapter{Continuation-based web programming}
\label{chap:continuations}
%include continuations.lhs
\chapter{Generic views}
\label{chap:views}
%include views.lhs
\chapter{Example: a Quiz system}
\label{chap:quizexample}
%include quiz.lhs

\chapter{Conclusion}
\label{chap:conclusion}
%include conclusion.lhs


\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}
