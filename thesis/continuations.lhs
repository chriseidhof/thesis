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

Web programming works over HTTP, which is a stateless protocol.
When web programmers need to model stateful programs that consist of multiple
pages, they need a way to keep track of state.
However, in normal web programming a user needs to do this manually by storing
state in a server-side session or serializing state and pass it around in the
URLs.

% Why is it important?
A lot of web applications involve workflows that span over multiple pages. By
storing state in the session and manually encoding workflows it is easy to make
an error. 
Programs like this become large and are hard to manage. 
Also, the programmer needs to manually translate between the specification and
the implementation.

% Our solution

Our solution is to use first-class functions to model stateful web programs
\cite{restruct}.
Such a first-class function is sometimes called a continuation: it represents
the function that is to be executed  and the environment
that it carries along. The environment is used to store state.

Let us give an example of a program where continuations are a good solution.
The Arc Challenge, as posed by \citet{arc}, was introduced to compare the
expressiveness of web programming languages. It is not a contrived problem,
web applications often need to solve these kinds of problems, where results
from a previous are used on a later page.


\begin{quote}
Write a program that causes the url \texttt{said} to produce a page with an input field and a submit button. When the submit button is pressed, that should produce a second page with a single link saying ``click here.'' When that is clicked it should lead to a third page that says ``you said: \dots'' where \dots\ is whatever the user typed in the original input field. The third page must only show what the user actually typed. I.e. the value entered in the input field must not be passed in the url, or it would be possible to change the behavior of the final page by editing the url.
\end{quote}

The solution in Arc, which gave its name to the challenge, shows the power of using continuations for web
programming.
After the link |"click here"| is clicked, a function
is executed that refers to values filled in by the user in the form
on the first page without having to pass these values around explicitly.

\begin{verbatim}
(defop said req
  (aform 
    [onlink "click here" 
     (pr "you said: " (arg _ "foo"))]
    (input "foo") (submit)))
\end{verbatim}

The challenge inspired us to write a Haskell library\footnote{\url{http://gist.github.com/260052}}
implementing an embedded domain specific language for web programming which
enables us to express the minimal solution as:

> said = do  name <- input
>            link "click here"
>            display ("you said: " ++ name)

This program is statically type checked for logical errors, is more to the point than its Arc
counterpart, has no need for inventing superfluous names, and reflects the sequential sequence of events better; we cannot think of a
shorter way to express the solution without using libraries that  were
specifically designed to solve this Arc Challenge.

In section \ref{sec:monadic}, we introduce the library that is used to
implement the above example. We show the main shortcoming of this approach:
it is impossible to serialize Haskell functions.
When we restart our server all
client state is lost.

Therefore, we provide an alternative interface in section
\ref{sec:arrowbased}. 
It is based on arrows \cite{hughes2000generalising}, and arrow values can be serialized more easily.
However, arrows have a different problem: they put more burden on the user of
the library.

As our last example, in section \ref{sec:defunctionalization} we use an indexed monad to keep
track of the environment so that we can perform automatic defunctionalization
\cite{reynoldsdefunctionalization}. However, this approach also is limited in
its usefulness: when users of the library make a mistake, the type errors are
quite complex.

In the last sections, we provide future work and conclude. There is an appendix
with the interfaces for both the monadic and arrow-based library on page 
\pageref{sec:interfaces}.

\todo{contributions}

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

\newpage
\section{Defunctionalization with indexed monads}
\label{sec:defunctionalization}

%include continuations/Defunctionalization.lhs

\section{Conclusion}

We have investigated a monadic approach, an arrow-based approach and
defunctionalization.
The monadic approach has the simplest interface and is the
easiest to use.
However, it lacks serialization of values.

The arrow-based interface enables serialization, but requires more work from the
library user. Arrow notation alleviates that problem, but does require the
library user to learn new syntax. It is a bit more cumbersome to use than
monadic do-notation. We still think that it is the best option at the moment.

Defunctionalization is a promising technique, but Haskell is unfortunately not
yet equipped with the right meta-programming tools to do defunctionalization
without modifying the compiler. This is because defunctionalization needs to
inspect variable bindings, recursion and type information.
In a language like MetaML \cite{taha1997multi, sheard1998using, moggi-idealized}
this would be easier. In a recent discussion
\footnote{\url{http://comments.gmane.org/gmane.comp.lang.haskell.cafe/72693}} on
the haskell-cafe mailing-list several meta-programming alternatives have been
compared by their relative strengths.

\subsection{Related work}

Our monadic library was inspired by iTasks \cite{plasmeijeriTasks}, a web programming library in
the Clean language. In Haskell, the WASH framework \cite{thiemann2002wash}
provides a similar style of web programming, but unfortunately, the code suffers
from severe bitrot. The links programming lanugage \cite{cooper2006links} hides
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

We plan to extend the arrow-based library to support the serialization of
continuations and release it on hackage
\footnote{\url{http://hackage.haskell.org}}. We want to extend the library to
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
