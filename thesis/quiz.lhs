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

In this chapter we build an example application for creating and taking
simple multiple-choice quizzes.
All the code is available as literate
Haskell\footnote{\url{http://github.com/chriseidhof/thesis/tree/master/thesis/quiz/}}.
The application is built on top of the three libraries developed in this thesis, and
we refer to parts of the libraries when the need arises.
The purpose of this chapter is to give an example of how the libraries work together.
In the later chapters, we describe each library in great detail, both the interface and implementation.
The first library we use is for data modeling in Haskell.
It provides a way to encode a data model on a high level that is independent of the actual database that is used.
The second library is for building workflows that span over multiple pages.
The third library we use makes it easy to generate view code based on the structure of the data model by using generic programming.
It also includes functions to change how the code is generated.

We start by defining the data model for our quizzes in section
\ref{sec:quizmodel}.
Based on that data model we build the controller and view code in section
\ref{sec:quizcontroller}.

We expect the reader of this chapter to be familiar with Haskell.
We use some advanced concepts, however, it is not necessary to understand all the concepts in order to see the big picture.
We also expect some familiarity with web programming. The reader may wish to
read through this chapter rapidly rather than understand every detail.
For some concepts or functions we have included page references in the margin
that explain them in more detail.
To be complete, we have included all the code of this example at the end of this
chapter.

\section{The Quiz model}
\label{sec:quizmodel}

%include quiz/QuizModel.lhs

\section{The Quiz controller and views}
\label{sec:quizcontroller}

%include quiz/QuizUrl.lhs

%include quiz/Quiz.lhs

\newpage
\section*{The entire example}

For completeness, we have included the entire code (except for the imports).

%include quiz/AllCode.lhs

%if not thesis
\end{document}
%endif
