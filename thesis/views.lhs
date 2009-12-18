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
\title{Generic views for web programming}

\maketitle

\tableofcontents

%endif

\section{Simple generic views}

\section{Customization}

Idea: transform entity into another nominal value, generate HTML for that.
Newtypes generate custom HTML components.

%if not thesis
\end{document}
%endif