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

\chapter*{Abstract}

In this thesis we show how to do web programming in the statically typed Haskell
programming language, applying programming language research to the web
programming world.
We construct three libraries that encompass a framework for building database-driven web applications.


\setcounter{tocdepth}{1}
\tableofcontents

\chapter{Introduction}
%include introduction.lhs
\chapter{Example: a Quiz system}
\label{chap:quizexample}
%include quiz.lhs
\chapter{Entity Relationship Models}
\label{chap:ermodels}
%include ermodels.lhs
\chapter{Continuation-based web programming}
\label{chap:continuations}
%include continuations.lhs
\chapter{Generic views}
\label{chap:views}
%include views.lhs

\chapter{Conclusion}
\label{chap:conclusion}
%include conclusion.lhs

\chapter*{Acknowledgements}

First of all, I would like to thank Doaitse for supervising my work. His
comments have been very valuable, and I have learned a lot from him.
It has always been a pleasure to discuss my thesis work, the future of
functional programming and all other things.
It was an honour to have Doaitse as my thesis supervisor.

I am also very thankful to Andres for being my second supervisor.
He is always a source of inspiration and knowledge, and has been very helpful.

Eelco has been both a great friend and business partner in the last few years.
He showed me how to work very hard and have fun at the same time.
Although we now will both go our own ways, I have had lots of fun and learned a lot from him.

I would like to thank the software technology staff and students for providing
a great environment to study in.
I have enjoyed both the courses and thesis work very much, thanks to them.
In particular, I would like to thank Sebastiaan, Tom and Erik for the times we
have worked together and all the interesting discussions we have had.

I would like to thank my parents for supporting me.
They have always believed in me, even though I can not explain to them what it
is that I have been doing.

Finally, I would like to thank Janneke for her love and support.


\newpage

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}
