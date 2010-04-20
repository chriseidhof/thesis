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
\title{Example application: a Quiz system}

\maketitle

\tableofcontents

%endif


\section{Introduction}

In this chapter we will build an example application for creating and taking
simple multiple-choice quizzes.
All the code is available as literate Haskell.
The application is built on top of the three libraries used in this thesis, and
we will give references to libraries where necessary.

We will start by defining the data model for our quizzes in section
\ref{sec:quizmodel}.
Based on that data model we will build the controller and view code in section
\ref{sec:quizcontroller}.

\section{The Quiz model}
\label{sec:quizmodel}

%include quiz/QuizModel.lhs

\section{The Quiz controller and views}
\label{sec:quizcontroller}

%include quiz/QuizUrl.lhs

%include quiz/Quiz.lhs

%if not thesis
\end{document}
%endif
