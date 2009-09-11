\documentclass[a4wide,12pt]{article}
%include polycode.fmt 
%format :-> = "\mapsto"
%format +++ = "\oplus"
% vim:spell
\usepackage{a4wide}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage{palatino}
\usepackage{rotating}

\begin{document}
\author{Chris Eidhof}
\title{Thesis Proposal: The Pesto framework \\
\normalsize{Building generic web interactions in Haskell}
}

\maketitle
\tableofcontents

\section{Introduction}

Web applications typically contain a lot of generated code that is based on the
underlying ER-model. By generating the code the model can be kept flexible.
However, oftentimes the generated code is edited and the flexibility is lost:
changes in the model have to be encoded by hand. In this thesis proposal we will
look at a way to keep the model flexible while still generating the code.

By using updateable views on our model we want to keep the handwritten code to a
minimum. Instead of giving the implementation of the user interface we will give
a description and then generate the interface. This has the advantage that no
manual HTML needs to be written. Because our application is written down as a
functional specification instead of as an implementation, we can easily generate
an application for other platforms (such as mobile phones) too.

Our work is inspired by contemporary web application frameworks such as Ruby on
Rails.  They provide the user with a quick way to build and generate
database-driven web applications. In all of these frameworks, however, the
generated code is typically edited after generation. This means that changes to
the model have to manually be kept in sync with the views.

In the next chapter we will introduce some of the necessary vocabulary and
background for our work. In the second chapter we will state our research
question and the approach. We will conclude with the expected results and a
planning.

\section{Context}
\subsection{Workflows}

Workflow systems describe how humans and computers interact. A \emph{workflow process
definition} describes which activities are needed to complete a certain business
process. These activities can be either human (for example, filling in a form,
confirming a message) or done by the computer (storing a record to the database,
calculating a result). A workflow process definition not only specifies the
activities, but also in what order they need to be executed.

A workflow management system is a tool to build executable specifications of
workflows. It is often a standalone product, and sometimes integrated into a
programming language. This distinction can make a large difference in the
\emph{expressive power} of a workflow management system.

The Workflow Patterns initiative has built a set of commonly recurring patterns
for workflow processes. They are similar to design patterns used in software
development. Not all workflow management systems supports all patterns but
sometimes, depending on the expressive power of such a system, it can be
extended to support additional patterns.

Sometimes, when designing workflow applications, the workflow is implemented
without a workflow management system. For example, it can be implemented in an
ad-hoc way as a web application. This can make it very hard to support
additional platforms, such as mobile devices. By using the right workflow
management system an implementor can abstract over platforms.

Also, a workflow system has to interact with the user and other systems or
processes. Much of this interaction is based on the domain of the problem. In
section \ref{domaindriven} we will see how a domain model can help keeping
workflow applications flexible.

% implementation approaches
%   what's already there? what are the (dis)advantages?

\subsection{Domain specific languages}

A \emph{domain specific language} (DSL) is a programming language designed to express
solutions in a specific domain. It is the opposite of a general-purpose
programming language. A DSL can be designed to help model a certain class of
problems more directly. At the very minimum, a DSL consists of a set of
\emph{symbols} and a set of rules (syntax) that describe how to combine these
symbols into well-formed expressions.

Symbols in a DSL can take many forms. For example, the syntax might closely
resemble a programming language. Alternatively, in workflow languages, the
modeling of the workflow is often done graphically.  The symbols are the
elements used to draw a workflow graph.

The solution expressed using a DSL can be used for several purposes. Often, it
is given a semantics by interpreting it or compiling it into executable code.
Also, a tool can check for well-formedness (with regard to the syntax) and
correctness (with regard to the semantics) of the expressed solutions.

\subsubsection{Embedded domain specific languages}

When a DSL is integrated into another language, it is called an \emph{embedded
domain specific language} or EDSL. Here, the symbols of the DSL are also symbols in the
\emph{host language}. By embedding the DSL into another language, the DSL will
inherit all kinds of properties of that language. 

Using all the features of the host languages gives a DSL a great amount of
interoperability with other libraries. Also, by using the type system of the
host language (when applicable), some properties can be stated about programs
written in the EDSL.  We will elaborate on this in later sections.

As an example, in the Haskell programming language, there are several libraries
for describing parsers. These libraries provide a number of symbols that can be
combined to write parsers. They use Haskell's type system to describe what the
result of parsing might be.

\subsection{Writing libraries as EDSLs}

When implementing embedded domain specific languages there are a lot of design
choices to be made. We choose Haskell as our host language. This gives us a lot
of things for free, such as automatic sharing of values and a type system. We
will discuss the problems that arise when using sharing and how we can use
Haskell's type system to our advantage. Finally, we will discuss abstraction for
stateful computations.

\subsubsection{Observable Sharing}

Due to the laziness of Haskell it is possible to construct values that can be
infinitely unfolded. For example, consider the following Haskell expression
which is visualized in Figure \ref{graphfig}:

\begin{code}
data Graph = Branch Graph Graph | Leaf Int
infiniteGraph = Branch (Leaf 1) infiniteGraph
\end{code}

\begin{figure}
\includegraphics[width=2.5cm]{reify}
\caption{A visual representation of the graph}
\label{graphfig}
\end{figure}

The expression |infiniteGraph| is a finite representation of an infinite value.
When analyzing the value, we can endlessly keep unfolding the recursive call to
|myTree|. An alternative way of representing this graph is as following:

\begin{code}
newtype Ref    =  Ref Int -- A reference to a graph expression
data Graph     =  Branch Ref Ref | Leaf Int
infiniteGraph  =  [(Ref 1, Branch (Ref 2) (Ref 1))
                  ,(Ref 2, Leaf 1)
                  ]
\end{code}

Here, we have made sharing explicit by replacing all recursive positions with
references. However, writing down graphs in this way is error-prone and a lot
more verbose. For example, it is possible to construct a Graph expression that
refers to a non-existing graph.

Sometimes, we want to specify our graphs using ordinary recursion, as in the
first example, but we would like to be able to convert them into the second
representation, where recursion and sharing is made explicit. By using
techniques for observable sharing we can achieve this. It has first been
introduced to the Haskell community by Claessen and Sands \cite{sharing} and has
been refined over the years. The solution proposed by Gill \cite{reify} has
virtually no impact on the user of the DSL. A user writes down the graph in a
style natural to Haskell.

\subsubsection{Lifting on Haskell's type system}

In this paragraph we will show how advanced features of Haskell's type system
can be used to make stronger guarantees about the correctness of our programs.
For example, when designing an embedded expression language, we could use the
following approach:

\begin{code}
data Exp = Num Int | Boolean Bool | If Exp Exp Exp | Add Exp Exp
\end{code}

However, using this datatype, it is possible to construct an expression that
adds two Boolean expressions. In general, this is not desirable. We would like
to prove statically that every expression is well-typed. By using GADTs
\cite{gadts}, we can restrict the expressions to be well-typed:

\begin{code}
data Exp a where
  Num      :: Int       -> Exp a
  Boolean  :: Bool      -> Exp Bool
  If       :: Exp Bool  -> Exp a    -> Exp a -> Exp a
  Add      :: Exp Int   -> Exp Int  -> Exp Int
\end{code}

This datatype gives a lot more guarantees about the well-typedness of its
expressions. GADTs can help both the designer and user of an EDSL to guarantee
correctness of the EDSL and the programs written with it.

\subsection{Control abstraction}

When describing stateful computations in Haskell there are a number of
abstraction mechanisms available. The most commonly used abstractions are
arrows, monads and applicative functors (or idioms). When implementing an EDSL
that does stateful computation, it can be interesting to use one or more of
these abstractions.

The three abstractions in the previous paragraph are not equally powerful. On
his website, Cooper \cite{cooperblog} gives a very clear example of the
the relative strengths. When using monads you can use an intermediate result at
any future step:

\begin{code}
do  x <- compOne
    y <- compTwo
    z <- compThree x
    return (x + y + z)
\end{code}

However, when using arrows, you can only use every intermediate result in the
step that's directly following. By using arrow combinators it is still
possible to thread values through to the next expression, but this has to be
done explicitly.

\begin{code}
do  x <- compOne
    y <- compTwo x
    return y -- You are not allowed to use 'x' here.
\end{code}

Applicative functors restrict this even further, you can only use intermediate
results in a final return clause:

\begin{code}
do  x <- compOne
    y <- compTwo
    return (x + y)
\end{code}

Choosing one of these abstractions has great impacts on library design, as we
will see in the next paragraph.


\subsection{Model Driven Development}
\label{domaindriven}

The domain model describes the entities and relations in a problem domain. In
model driven development, the domain model is kept close to concepts in the
problem domain.  This opens up possibilities for non-technical people to
understand part of the application development.

As new insights arrive from communicating with the stakeholders in the
application development process, the model changes. From our experience,
we know that it takes a couple of iterations before the domain model arrives at
its final state. Therefore, we believe that it is vital to keep the domain model
as flexible as possible.

\subsection{Generic Programming}

Using datatype-generic programming we can write functions that operate on the
structure of data.  In a language with a nominative type system, generic
programming often does not come for free. A value first needs to be converted
into a structural view before it can be processed by generic functions.
Afterwards, it can be converted back. An \emph{embedding projection pair} is the
pair of functions that does these conversions.

Generic programming can be used to keep the domain model flexible. If we specify
our domain model as nominative types and derive the embedding projection pair,
we can do generic programming on the domain model. We can build a lot of useful
generic functions: a generic database interface, generic forms, XML generation,
to name a few. 

We are not the first to envision this. In recent research, iData has done this
in the Clean programming language. iData is a library of generic functions that
generates forms and offers serialization (in files or a database) of values.
However, it is not clear if it is easy to write custom functions in iData.

\subsection{Updatable views}

Updatable views are a problem that has been studied extensively in both
literature and practice. When we have a function $f$ that is a view on arguments
of type $C$: $f : C \to A$. An updatable view also has a function $f^{-1}$ that
has type $f^{-1} : A \times C \to C$. As an example, consider the function $fst$
on pairs: $fst : A \times B \to A$. We say that $A$ is a view of the original
pair. Now consider the function $fst^{-1} : A \times (A \times B) \to A \times
B$ that updates the first element in the pair. The combination of $fst$ and
$fst^{-1}$ is called a \emph{lens} (TODO cite) and turns $A$ into an updatable
view. (TODO: this is just a sketch, citations needed and talk about rdbm systems).

\section{Our research}

\subsection{Research Question}

Our main question is: \emph{How far can we push the declarative way of building
applications?}. In order to answer this, we will investigate the following
subquestions:
\begin{itemize}
\item What are the limits of can we use updateable views and generic programming?
\item What are the right abstractions for control flow?
\end{itemize}

\subsection{Our approach}
\label{approach}

We are going to build two orthogonal embedded domain specific languages in the
Haskell programming language. The first EDSL will focus on the combination of
datatype-generic programming and updateable views for web development. The
second EDSL will be a way of declaring control flow for application programming.
To make sure our libraries are powerful enough for expressing real world
problems we will port an existing PHP application.

\subsection{Domain model: generic programming}

Our domain model is described as an entity-relationship model. For example, when
describing a weblog, our model might contain entities for users, posts and
comments. When building an application, we need to persist the domain model,
visualize it and maybe provide users of the application with an API for it.

A lot of these functions work on the structure of a domain model. For example,
if we generate a form for editing a post on the weblog we can derive this form
solely from the structure of the entity. One way of doing this structural
programming is by using a generic programming library.

In Haskell, we could define our domain model for a weblog like this:

\begin{code}
data Post       = Post    {title :: String, body :: String, author :: BelongsTo User, comments :: HasMany Comment}
data Comment    = Comment {text :: String, date :: DateTime, author :: BelongsTo User}
data User       = User    {name :: String, password :: String, age :: Int}
\end{code}

Here, we have annotated the relationships using special types such as
|BelongsTo| and |HasMany|. These are used to encode the kind of relationship
(one-to-one, one-to-many, etc.). We will expand upon this in a later section.

\subsubsection{Structural representation}

In order to write generic functions on our domain model, we need structural
representations (using a basic set of codes) and embedding projection pairs. The
combination of these codes and conversion functions is called a \emph{universe}.

Every generic programming library has different features and requirements. We
have used the comparison from Rodriguez et al. (todo cite) to evaluate a number of
libraries. We built a prototype generic programming library that was based on
\emph{EMGM} (todo cite) and we have looked at \emph{regular}, which is recent
work by Van Noort et al.

TODO: explain why we chose regular (only single-constructor record types)

\subsubsection{Generic views and forms}

Using the regular library we can derive the structure of a datatype. For
example, the |User|-datatype described above looks like this:

\begin{code}
data User       = User    {name :: String, password :: String, age :: Int}
\end{code}

When deriving the structural view using the \emph{regular} library, we get a
function |from| that converts a value of type |User| into the structural
representation:

\begin{code}
from :: User -> PF User User
\end{code}

When we expand the type on the right-hand side of the arrow, we get a type that
looks like this:

\begin{code}
from :: User 
     -> (C User_User_  
           (       S User_User_name_      (K String)
              :*: (S User_User_password_  (K String)
              :*: (S User_User_age_       (K Int)))
           )
           User
           )
\end{code}

Here we can see that the structural type is a constructor with a nested product
type. Within the product type there is an |S|-type that describes the
label and finally a |K|-type that refers to the type of a field.

We can now write a function that generically generates an HTML view.

\begin{code}
class    Html  a       where html :: a -> X.Html
instance Html  Int     where html = X.toHtml . show
instance Html  String  where html = X.toHtml 

class GHtml f where
  ghtmlf :: (a -> X.Html) -> f a -> X.Html

instance (Constructor c, GHtml f)  => GHtml (C c f)    where
  ghtmlf f cx@(C x)   = (X.h1 << capitalize (conName cx)) +++ ghtmlf f x
instance Html a                    => GHtml (K a)      where
  ghtmlf _ (K x)      = html x
instance (GHtml f, GHtml g)        => GHtml (f :*: g)  where
  ghtmlf f (x :*: y)  = ghtmlf f x +++ X.br +++ ghtmlf f y
instance (Selector s, GHtml f)     => GHtml (S s f)    where
  ghtmlf f s@(S x)    = X.label << ((h s) ++ ": ") +++ ghtmlf f x

ghtml :: (Regular a, GHtml (PF a)) => a -> X.Html
ghtml x = ghtmlf ghtml (from x)
\end{code}

The type signature for the |ghtml| function states that if can convert |a| into
a structural representation and if it can generate HTML for that structure then
it can generate HTML for something of type |a|.

As an example, we can run ghtml on a value of the User datatype:

\begin{code}
ghtml $ User "chris" "test" 24
\end{code}

As output we will get some HTML:

\begin{verbatim}
<h1>User</h1>
<label>Name: </label> chris<br />
<label>Password: </label> test<br/>
<label>Age: </label> 24
\end{verbatim}

While this is great, we often want to slightly modify the HTML that is
generated. For example, we might not want to include the password field. As far
as we know, all frameworks for web programming require you to write custom HTML
at this point. However, we would like to keep generating the HTML instead of
writing it by hand.

In order to do this we need a view on the User datatype and then generate the
HTML for it. For example:

\begin{code}
data UserView = UserView {name_ :: String, age_ :: Int}
toUserView :: User -> UserView
toUserView u = UserView (name u) (age u)
\end{code}

Before we generate HTML for a |User|, we will first convert it into a |UserView|
using the |toUserView| function and then generate the HTML.

Using the formlets library (TODO cite) we can also generate forms in a generic
way, much in the same way as we generate the HTML. A formlet consists of two
functions:

\begin{code}
runFormlet :: Formlet a -> (Maybe a -> X.Html, FormParser a)
\end{code}

The first function is used to generate the HTML for a form and it takes an
optional default value. The second function is to be used after the form is
submitted, and it tries to parse the HTTP request variables to construct a value
of type |a|. We can now generate forms for, for example, a User. 

This is where updatable views come into play. When we want to generate a view
of the |User|-datatype, we would like to update the original value. By using a
combinator library (TODO: cite) for defining updatable views we can achieve just
this.

\subsubsection{Generic database access}

Just like the views and forms above, we can generically derive methods to
persist entities on disk. Often this is done using a database. By writing a
number of small generic methods we can easily build a persistancy layer that
stores our model in a database. We have completed a prototype library that does
exactly this. In our research we want to explore this area further and see
whether we can apply more techniques from database programming.

\subsection{Control flow: a library for workflow control-flows}

Write about the control flow library. Using right abstraction (arrows). Explain
why we're not using Monads. Talk about Hughes's paper. Serializing state.
Transform control flow structure into 


\subsection{Porting an existing application}

By porting an existing application written in PHP to our framework we want to
compare both approaches on software quality metrics as described in The Art Of
Software Architecture \cite{taosa}:

\begin{itemize}
\item Functionality
\item Performance (efficiency)
\item Modifiability
\item Reliability
\item Usability
\item Portability
\end{itemize}

We expect to end up with an application with the same functionality, much better
performance (PHP is an interpreted language). Also, because Haskell allows for
more abstraction, we expect to end up with a lot less code, which is good for
modifiability. Because of the typechecker we expect that our program is more
reliable and will have less bugs. The usability should be the same, but because
of our declaritive way of building applications we expect that we can very
easily port it to other architectures.


% TODO:
% research statement?
% 

%----------------------------------------

% To give a concrete example, a workflow might look like this (written down in Haskell):
% 
% 
% \begin{code}
% \end{code}
% 
% This workflow presents the user with a task to enter her name, then a task to
% find the correspond e-mail address. Next it displays the address and it will
% return the name. These tasks are sequenced, and not every line requires user
% interaction. This is a typical interleaving of human tasks and computer tasks as
% can be found in workflows.
% 
% Building executable specifications is what workflow modeling tools are all
% about. However, execution should not be limited to just one platform, such as the web
% .  We envision a system where a workflow is an abstract description of a
% business process, and not mixed with implementation-specific code such as HTML
% fragments. For example, as mobile phones get more advanced, it would make a lot
% of sense to have a mobile version of a workflow application next to a web
% application. In other words, the workflow specification has to be
% \emph{platform-agnostic}.
% 
% When designing an application that contains the workflow of an organization,
% there are multiple stakeholders. Not all of these people might know how to read
% a specification in the form of a computer program. On the other hand, the
% implementor might have to deal with unclear or incomplete requirements. From our
% own experience, we know that a lot of misunderstanding can be prevented by
% integrating all stakeholders in the development process. In a
% workflow system, workflows can be modelled as graphs and thus have a direct
% visual representation. By drawing this graph automatically from the
% specification there is always an up-to-date and understandable representation of
% the system that can be communicated to non-technical users.
% 
% From a technical perspective, we believe that strong typing at every level is
% essential for building reliable software.  When implementing and analyzing
% transformation functions on the workflows, we would like the compiler to check
% that everything stays type-correct. By using a type system that is powerful
% enough to support features like GADTs, we can do typed transformations on the
% core datatypes.
% 
% Workflows are very much about the processes, i.e. the flow of the language.
% Another important part when modeling applications is the domain model. For
% rapid development and maintenance, it is important to keep the domain model
% flexible. This means that the domain-specific code should be kept to a minimum.
% Generic programming is a technique that can facilitate this. For example, using
% generic programming, we can automatically generate forms, overview pages,
% database interfaces and API interfaces. 
% 
% \subsection{Related work}
% 
% Orc \cite{orc} is a recent research language that is designed for asynchronous
% and concurrent applications, such as workflow modeling. Orc is an untyped
% language, and is run on the JVM. It is not meant as a general purpose
% language. We think that by embedding its functionality within a general purpose
% language like Haskell, it will be much easier to construct applications that use
% other libraries. Orc currently has no facility for generic programming.
% 
% iTasks \cite{iTasks} is a system built in Clean, a language quite similar to
% Haskell. It provided the main inspiration for this research proposal. However,
% iTasks is untyped at its very core. This makes it hard to reason about the
% correctness and reliability. Also, the sole product of a workflow description in
% iTasks is a web application. HTML is integrated in the process of describing
% workflows, which makes it very platform-specific.
% 
% There exist a number of commercial workflow modeling tools. Almost all of these
% provide just a visual interface for describing the workflows. We believe that a
% textual specification can be much more powerful, and that a visual graph should
% be the end-product of a workflow, not the way to design it.
% 
\section{Research question}

This thesis project will investigate what is needed to have a workflow modeling tool
where the implementor can build workflows in a composable way using the language
Haskell. We want to know: 

\begin{itemize}
\item How can we design a workflow modeling tool where a workflow specification is platform-agnostic?  
\item How can we design that system in such a way that it has a type-safe core language?  
\item How can we minimize application-specific code by using generic programming?
\end{itemize}

\subsection{Contribution}

Compared to other approaches, we will deliver a solution that is strongly typed
at the highest and lowest level. By using modern techniques like GADTs and
type-level programming we can build libraries that are fully type-checked using
the compiler. Also, we will build a number of libraries that can be used for
building workflow applications.

\section{Approach}

We propose to build a workflow modeling tool in Haskell that provides the user
with a way to express workflows in an implementation-agnostic way. We will port an
existing workflow application that is written in PHP to show how our system
compares against a manual implementation of a workflow application. 

Using this application as a starting point, we will try to keep the
application-specific code to a minimum by generalizing as much code as possible
into libraries. 

\subsection{The workflow library}
\subsection{Generic Programming library}
\subsection{Other libraries}

\section{Expected results}

We expect to end up with at least one application and two libraries. The first
library will be for building interactive workflows. The other library will
be for the generic programming on the domain model. It will provide facilities
for writing your own generic function as well as a number of default functions.

We suspect that the ported application will have less than half the LOC compared
to the original version. PHP provides not nearly as much mechanisms for
abstraction as Haskell. Also, we will end up with a fully type-checked version
of the application. Security is no longer an issue if we take the right
precautions at the type-level. Also, we expect the application to run much
faster than its PHP variant because we will compile it using an optimizing
compiler, whereas the PHP variant is interpreted.

\section{Planning}

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}

-- implementation issues
