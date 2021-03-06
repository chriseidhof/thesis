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
\usepackage{natbib}
\usepackage{todonotes}

%include formatting.lhs

\begin{document}


\author{Chris Eidhof}
\title{Continuation-based web programming}

\maketitle

\tableofcontents

%endif

\section{Introduction}

% What is the problem?

When web programmers model stateful programs that span multiple pages they need
a way to keep track of state, since HTTP is a stateless protocol.
This is usually by storing state in a server-side session or serializing the state and pass it around in the URLs.



% Why is it important?
Once the size of the state to be maintaned grows, this approach becomes cumbersome and very implicit: there are no static guarantees about which values are in the session.
Programs designed like this become complex and are hard to manage. 

% Our solution

In this chapter we show how to alleviate the usual shortcomings by resorting
to first-class functions that model stateful web programs
\cite{restruct}, and specifically their use as 
continuations, which both represent what has to happen next (the algorithm) and
the environment in which this has to happen.
The environment is used to store state.

To show how continuations come in handy, consider the Arc Challenge,
as posed by Graham \cite{arc}; it is introduced to compare the
expressiveness of web programming languages. The challenge is not a contrived
since web applications often need to solve such problems, where results
from a previous interaction are used in a later interaction.

\begin{quote}
Write a program that causes the url \texttt{said} to produce a page with an input field and a submit button. When the submit button is pressed, that should produce a second page with a single link saying ``click here.'' When that is clicked it should lead to a third page that says ``you said: \dots'' where \dots\ is whatever the user typed in the original input field. The third page must only show what the user actually typed. I.e. the value entered in the input field must not be passed in the url, or it would be possible to change the behavior of the final page by editing the url.
\end{quote}

The solution in Arc, which gave its name to the challenge, shows the power of using continuations for web
programming.
After the link |"click here"| is clicked, a function
is executed that refers to values filled in by the user in the form
on the first page. Note that there is no code that passes the value
returned by the first interaction to the final response:

\begin{verbatim}
(defop said req
  (aform 
    [onlink "click here" 
     (pr "you said: " (arg _ "foo"))]
    (input "foo") (submit)))
\end{verbatim}

The challenge inspired us to write a Haskell library\footnote{\url{http://gist.github.com/260052}}
implementing an embedded domain specific language for web programming, which
enables us to express the minimal solution even shorter:

> said = do  name <- input
>            link "click here"
>            display ("you said: " ++ name)

This program is statically type checked for logical errors, is more to the point than its Arc
counterpart, has no need for inventing superfluous names, and reflects the sequence of events better; we cannot think of a
shorter way to express the solution without using libraries that  were
specifically designed to solve this Arc Challenge.

In section \ref{sec:monadic}, we introduce the library that is used to
implement the above example. We show the main problem with this Haskell based approach:
it is impossible to serialize Haskell functions.
When we restart our server all
client state is lost.

Therefore, we provide an alternative interface in section
\ref{sec:arrowbased}. 
It is based on arrows \cite{hughes2000generalising}, which can be serialized more easily.
However, this comes at the price: programming with arrows puts a larger burden on the user of
the library.

% As our last example, in section \ref{sec:defunctionalization} we use an indexed monad to keep
% track of the environment so that we can perform automatic defunctionalization
% \cite{reynoldsdefunctionalization}. However, this approach also has its problems: 
% when a programmer using the library makes a mistake, the type errors are quite complex.

In the last sections, we provide future work and conclude. There is an appendix
with the interfaces for both the monadic and arrow-based library on page 
\pageref{sec:interfaces}.


\newpage
\section{A monadic approach}
\label{sec:monadic}

%include continuations/Monadic.lhs

\newpage
\section{An arrow-based approach}
\label{sec:arrowbased}

%include continuations/ArrowBased.lhs

\subsection{Serialization of Arrows}
\label{sec:arrowserial}
%include continuations/ArrowSerialize.lhs

%   \newpage
%   \section{Defunctionalization with indexed monads}
%   \label{sec:defunctionalization}
%   
%   %include continuations/Defunctionalization.lhs

\section{Conclusion}

We have investigated a monadic approach and an arrow-based approach.
The monadic approach has the simplest interface and is the
easiest to use.
However, it lacks serialization of values.

The arrow-based interface enables serialization, but requires more work from the
library user. Arrow notation alleviates that problem, but does require the
library user to learn new syntax. It is a bit more cumbersome to use than
monadic do-notation. We still think that it is the best option at the moment.


\subsection{Related work}

Our monadic library was inspired by iTasks \cite{plasmeijeriTasks}, a web programming library in
the Clean language. In Haskell, the WASH framework \cite{thiemann2002wash}
provides a similar style of web programming, but unfortunately, the code suffers
from severe bitrot. The Links programming language \cite{cooper2006links} hides
the notion of continuations from the user, and allows the user to construct web
programs using regular functions.

In dynamically typed languages, continuation-based web programming has been
around for quite some time \cite{ducasse2007seaside, restruct,
krishnamurthi2007implementation}.
Because of the dynamic natures of their languages, these libraries can serialize
and deserialize continuations easily.
However, as stated in the introduction, their programs are not statically
checked by the compiler, and do not have the advantage of having type-inference.

\subsection{Future work}

Defunctionalization is a technique to convert higher-order functional programs
into first-order programs, i.e. programs without higher-order functions
\cite{reynoldsdefunctionalization, danvy2001defunctionalization}. We have
extended our monadic library to perform automatic defunctionalization by
changing the monad to an indexed monad. However, this approach quickly became
too complex, both the implementation and the interface: we were unable to hide the
complexity from the library users.
Defunctionalization is a promising technique, but Haskell is unfortunately not
yet equipped with the right meta-programming tools to do defunctionalization
without modifying the compiler. This is because defunctionalization needs to
inspect variable bindings, sharing and type information.
In a language like MetaML \cite{taha1997multi, sheard1998using, moggi-idealized}
or FreshML \cite{shinwell2003freshml, shinwell2002freshml}
this would be easier. In a recent discussion\footnote{\url{http://comments.gmane.org/gmane.comp.lang.haskell.cafe/72693}} on
the haskell-cafe mailing list several meta-programming alternatives have been
compared by their relative strengths.

We plan to extend the arrow-based library to support the serialization of
continuations and release it on hackage
\cite{coutts2008haskell}. We want to extend the library to
support more primitive actions that can also be user-defined.

We hope that a new typed meta-programming language for Haskell arises that
enables us to do defunctionalization of monadic |Web| values. That way we can
provide a very simple but powerful interface to the library users while being
able to serialize functions.

\section*{Library interfaces}
\label{sec:interfaces}

\subsection{The monadic interface}
\label{sec:monadinterface}

%include continuations/MonadicInterface.lhs

\subsection{The arrow-based interface}
\label{sec:arrowinterface}

%include continuations/ArrowBasedInterface.lhs

%if not thesis

\bibliographystyle{plain}
\bibliography{bibliography}


\end{document}
%endif
