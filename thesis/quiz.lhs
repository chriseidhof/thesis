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

In this chapter we will use all three libraries above and build a system for
creating and taking quizzes.
A quiz contains a description and questions.
If a user decides to take a quiz, she has to fill in her name and answer every
question.
In section \ref{sec:quizmodel} we build the ER-model for our quizzes. Then, in
section \ref{sec:quizcontroller} we build the controller and view code.

\section{The Quiz model}

%include quiz/QuizModel.lhs

\section{The Quiz controller and views}

%include quiz/QuizUrl.lhs

%include quiz/Quiz.lhs

%if not thesis
\end{document}
%endif
