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

% structure
% Introduction
% Summary
% Overview of relevant literature
% Problem definition
% Work plan
% Execution of the work plan: solutions, results, elaborations etc.
% Conclusions
% Bibliography

\section{Personal motivation}

This is the proposal for the research done as my Master's graduation project.
Before I started the Software Technology Master I did a lot of web programming.
I had seen a couple of frameworks, and while quite powerful, I always felt that
they weren't really beautiful. Something didn't feel right.

During the courses of my Master's I got taught more and more techniques. With
almost everything that I learned I thought: how can I apply this to make web
programming better or easier? In my research I will try to apply the
techniques I learned, such as generic programming, lazyness, strong typing and
lenses, to web programming.

Another thing I've learned from the environment I was in during my Master's:
start out with a beautiful model that's mathematically correct and then see how
you can implement it. It might take some time, but a good implementation will
almost always follow. I will try to do this during my research as well.

\section{Introduction}

Most database-driven applications contain a large amount of code that is
directly based on the underlying ER-model. Typically, the code is
platform-specific and partly generated. Sometimes the generated code is edited
by hand to achieve customization.

We propose a system for building database-driven applications that is completely
declarative: we want to achieve a level of abstraction that is
platform-independent. From a single description, it should be easy to generate
both a web application as well as an application for mobile phones.  The
programmer doesn't have to know about the underlying implementation but builds
graphical user interfaces and defines control flow using a high level of abstraction.

Our work is inspired by contemporary web application frameworks such as Ruby on
Rails and iTasks. These frameworks are written for a specific platform: the web.
We want to abstract away from that platform and write applications for a number
of platforms using the same techniques. Our architecture will be based on the
model-view-controller pattern: we believe this gives a good separation of
concerns.

By using updateable views (also called lenses) on the ER-model we want to keep the
platform-specific code to a
minimum. Instead of giving the implementation of the user interface we will give
an abstract description of the GUI and then generate the interface. This has the
advantage that no manual GUI code needs to be written. Because applications
are written down as a functional specification instead of as a direct implementation,
we can easily target multiple platforms.

The controller part ties the model and the views together, and defines the
control flow of the applications.
There are already extensive studies done on this subject, yielding libraries
such as iTasks \cite{iTasks} and programming languages such as Orc \cite{orc}.
We will try to identify recurring patterns in existing applications and base our
work on that.  When designing the control-flow part, we can use abstractions
such as monads or arrows: as we will see later, choosing the right abstraction
is important for both expresiveness and ease of use.

In order to describe our ER-model we will analyze existing models and see how
we can represent those in Haskell. We believe it should be easy for the
programmer to change the model without having to change a lot of the code. We
want to achieve this by writing code based on the structure of the model using a
technique called generic programming.

Our work will be implemented in Haskell.  In order to do that, we will build a
number of libraries. Some of the libraries will be in the form of an embedded
domain specific language. Using these libraries we will port an existing
application to see how good our libraries work for real work.

In the next chapter we will introduce some of the necessary vocabulary and
background for our work. In the second chapter we will state our research
question and our approach. We will conclude with the expected results and a
planning.

\section{Context}

\subsection{Model Driven Development}
\label{domaindriven}

The ER model describes the entities and relations in a problem domain. In
model driven development, the model is kept close to concepts in the problem
domain (as opposed to algorithmic concepts). The models are iteratively designed
through communication with all stakeholders. We believe it is vital to keep the
domain model as flexible as possible: a new iteration of the model should not
mean that a large amount of code has to be rewritten.

\subsection{Generic Programming}

Using datatype-generic programming we can write functions that operate on the
structure of data.  In a language with a nominative type system, generic
programming often does not come for free. A value first needs to be converted
into a structural view before it can be processed by generic functions.
Afterwards, it can be converted back. An \emph{embedding projection pair} is the
pair of functions that does the conversion to and from a structural view.

Generic programming can be used to keep the domain model flexible. If we specify
our domain model as nominative types and derive the embedding projection pair,
we can do generic programming on the domain model. We can already envision a lot
of useful generic functions: a generic database interface, generic forms and XML
generation, to name a few. 

We are not the first to think of this. In recent research, iData has done this
in the Clean programming language. iData is a library of generic functions that
generates forms and offers serialization (in files or a database) of values.
However, it is not clear if it is easy to write custom functions in iData.


% implementation approaches
%   what's already there? what are the (dis)advantages?

\subsection{Domain specific languages}

A \emph{domain specific language} (DSL) is a language designed to express
solutions in a specific domain. In some ways, it is the opposite of a general-purpose
programming language. A DSL can be designed to help model a certain class of
problems more directly. At the very minimum, a DSL consists of a set of
\emph{symbols} and a set of rules (syntax) that describe how to combine these
symbols into well-formed expressions.

Symbols in a DSL can take many forms. For example, the DSL might closely
resemble a programming language. Alternatively, in control-flow languages, the
modeling of the control-flow is often done graphically.  The symbols are the
elements used to draw a control-flow graph.

A problem solution expressed using a DSL can be used for several purposes. Often, it
is given a semantics by interpreting it or compiling it into executable code.
On the other hand, a tool can check for well-formedness (with regard to the
syntax) and correctness (with regard to the semantics) of the expressed
solutions.

\subsubsection{Embedded domain specific languages}

When a DSL is integrated into another language, it is called an \emph{embedded
domain specific language} or EDSL. Here, the symbols of the DSL are also symbols in the
\emph{host language}. By embedding the DSL into another language, the DSL will
inherit all kinds of properties of that language. 

Being embedded in a host languages gives a DSL a great amount of
interoperability with other libraries. Also, by using the type system of the
host language, some properties can be stated about programs
written in the EDSL.  We will elaborate on this in later sections.

As an example, in the Haskell programming language, there are several libraries
for describing parsers. These libraries provide a number of symbols that can be
combined to write parsers. They use Haskell's type system to describe what the
result of parsing will be.

\subsection{Writing libraries as EDSLs in Haskell}

When implementing embedded domain specific languages there are a lot of design
choices to be made. In the rest of this proposal, we will use Haskell as our host
language. This gives us a lot of things for free, such as automatic sharing of
values and a type system. We will discuss the problems that arise when using
sharing and see how we can use Haskell's type system to our advantage. Finally,
we will discuss abstractions for stateful computations.

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
itself. An alternative way of representing this graph is as following:

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
refers to a non-existing node.

Ideally, we want to specify our graphs (such as control-flow descriptions) using
ordinary recursion, as in the first example, but we would like to be able to
convert them into the second representation, where recursion and sharing is made
explicit. By using techniques for observable sharing we can achieve this. It has
first been introduced to the Haskell community by Claessen and Sands
\cite{sharing} and has been refined over the years. The solution proposed by
Gill \cite{reify} has virtually no impact on the user of the DSL. A user writes
down the graph in a style natural to Haskell.

\subsubsection{Lifting on Haskell's type system}

In this paragraph we will show how advanced features of Haskell's type system
can be used to make stronger guarantees about the correctness of our programs.
For example, when designing an embedded expression language, we could use the
following approach:

\begin{code}
data Exp = Num Int | Boolean Bool | If Exp Exp Exp | Add Exp Exp
\end{code}

However, using this datatype, it is possible to construct an expression that
adds two Boolean expressions. Generally this is not desirable. We would like
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
correctness of the EDSL and the programs written with it. In our thesis, we want
to make use of the type system in such a way that it prevents us from writing
faulty expressions.

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
extended to support additional patterns. The number of patterns supported is
another good indication of the expressive power.

Sometimes, when designing workflow applications, the workflow is implemented
without a workflow management system. For example, it can be implemented in an
ad-hoc way as a web application. This can make it very hard to support
additional platforms, such as mobile devices. By using the right workflow
management system an implementor can abstract over platforms.

\subsection{Control abstraction}

When describing stateful computations in Haskell there are a number of
abstraction mechanisms available. The most commonly used abstractions are
arrows, monads and applicative functors (also called idioms). When implementing an EDSL
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

Choosing one of these abstractions has great impact on library design, as we
will see in the next paragraph.

\subsection{Updatable views}

Updatable views are a problem that has been studied extensively in both
literature and practice. When we have a function $f$ that is a view on arguments
of type $C$: $f : C \to A$. An updatable view also has a function $f^{-1}$ that
has type $f^{-1} : A \times C \to C$. As an example, consider the function $fst$
on pairs: $fst : A \times B \to A$. We say that $A$ is a view of the original
pair. Now consider the function $fst^{-1} : A \times (A \times B) \to A \times
B$ that updates the first element in the pair. The combination of $fst$ and
$fst^{-1}$ is called a \emph{lens} (TODO cite) and makes $A$ an updatable
view of $C$. The $fst$ is called the \emph{get} function and the $fst^{-1}$ is called
the \emph{putback} function.

This concept looks quite theoretical, but fits very well with model-driven
development. We can present a projection of our original model to our user,
where meta-information and sensitive information is removed. The user can then
edit that projection and the original model gets updated. This is indeed very
much like views in a relational database.

\section{Our research}

\subsection{Research Question}

When we started out, our original question was: \emph{How can we build
database-driven applications more easily?} In order to answer this, we will analyze existing
database-driven applications to identify recurring patterns.

Most of these applications can be modeled using the model-view-controller
paradigm. The model is in this case the ER-model, the view is the presentation
layer (e.g. the GUI or the API) and the controller is the glue code between the
model and the presentation. In each of these three layers we want to identify
common patterns. By starting out with analyzing the functionality and GUI in the
applications, we want to get a top-down analysis of common patterns. By
researching existing applications, we want to answer the following questions:

\begin{itemize}
\item What are commonly used interface elements?
\item What is common functionality in database-driven applications?
\item What does the ER-model look like for these applications?
\end{itemize}

The answers to these questions will give us a set of view patterns, controller
patterns and model patterns. The next part of our research will be about
implementing these patterns in Haskell libaries, which leads to the following
questions:

\begin{itemize}
\item What are the right abstractions for views?
  \begin{itemize}
  \item How do they relate to the model?
  \item How can we automatically derive (updateable) views from the model?
  \item How can we specialize these views?
  \item How can we model views in an implementation-independent way?
  \end{itemize}
\item What are the right abstractions for control flow and business logic?
  \begin{itemize}
  \item How can we use Haskell's control flow abstractions to our advantage?
  \item Can we reuse existing libraries for control flow?
  \item What are the consequences of using combinators like iTasks or Orc?
  \end{itemize}
\item What are the right abstractions for the model?
  \begin{itemize}
  \item How can we persist the model using a database?
  \item What types of relations can we model?
  \item How does this relate to existing theory?
  \end{itemize}
\end{itemize}

Finally, we want to test out the code we have written by implementing an
application:

\begin{itemize}
 \item How can we use our framework for modeling an existing application?
 \item What are the limits of our framework compared to hand-written code?
 \item How does our framework compare in code size and speed?
\end{itemize}

\subsection{Our approach}
\label{approach}

We are going to build two orthogonal embedded domain specific languages in the
Haskell programming language. The first EDSL will focus on the combination of
datatype-generic programming and updateable views for web development. The
second EDSL will be a way of declaring control flow for application programming.
To make sure our libraries are powerful enough for expressing real world
problems we will port an existing PHP application.

To write an application, the user defines her ER-model and uses the functions
from the generic programming library to build forms, views and other GUI
elements. The resulting screens are then composed using the control flow
library. This \emph{application description} can then be compiled for specific
platforms. Figure \ref{overallarch} depicts the architecture of such an
application. On top is the library code. Inside the green box is the actual
application specification which consist (at the very least) of an ER-model with accompanying
functions and the application flow. The application flow uses the control flow
library and other libraries. The ER-model uses the generic programming library
for ER-models to define itself and to get useful functions.

From the application specification we can generate products such as a mobile
application, a web application or a desktop application.

\begin{figure}
\includegraphics{architecture/overall}
\caption{The overall architecture}
\label{overallarch}
\end{figure}

\subsection{Domain model: generic programming}

Our domain model is described as an entity-relationship model. For example, when
describing a weblog, our model might contain entities for users, posts and
comments. When building an application, we need to persist the domain model,
visualize it and maybe provide users of the application with an API for it, as
can be seen in figure \ref{gpfig}.

A lot of these functions work on the structure of a domain model. For example,
if we generate a form for editing a post on the weblog we can derive this form
solely from the structure of the entity. One way of doing this structural
programming is by using a generic programming library.

\begin{figure}
\includegraphics{architecture/generic-programming}
\caption{Generic programming}
\label{gpfig}
\end{figure}

In Haskell, we could define our domain model for a weblog like this:

\begin{code}
data Post       = Post     {  title :: String, body :: String
                           ,  author :: BelongsTo User, comments :: HasMany Comment}
data Comment    = Comment  {  text :: String, date :: DateTime, author :: BelongsTo User}
data User       = User     {  name :: String, password :: String, age :: Int}
\end{code}

An entity description (such as |Post|) contains both properties (simple values)
and relations (|BelongsTo User|, |HasMany Comment|).

We have annotated the relationships using special types such as
|BelongsTo| and |HasMany|. These are used to encode the kind of relationship
(one-to-one, one-to-many, etc.). They will also serve as an indicator for the
database layer about where to store the foreign keys.

\subsubsection{Structural representation}

In order to write generic functions on our domain model, we need structural
representations (using a basic set of codes) and embedding projection pairs. The
combination of these codes and conversion functions is called a \emph{universe}.

Every generic programming library has different features and requirements. We
have used the comparison from Rodriguez et al. (todo cite) to evaluate a number of
libraries. We built a prototype generic programming library that was based on
\emph{EMGM} (todo cite).  We have also built a prototype of our library based on
regular.

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
class     Html  a       where html :: a -> X.Html
instance  Html  Int     where html = X.toHtml . show
instance  Html  String  where html = X.toHtml 

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

The type signature for the |ghtml| function states that if |a| can be converted into
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

While this is useful, we often want to slightly modify the presentation.
For example, we might not want to include the password field or use a
textarea instead of a regular textfield. As far as we know, all frameworks for
web programming require you to write custom HTML at this point. However, we
would like to keep generating the HTML instead of writing it by hand.

In order to do this we need a view on the User datatype and then generate the
HTML for it. For example:

\begin{code}
data UserView = UserView {name_ :: String, age_ :: Int}
toUserView :: User -> UserView
toUserView u = UserView (name u) (age u)
\end{code}

Before we generate HTML for a |User|, we will first convert it into a |UserView|
using the |toUserView| function and then generate the HTML. There are obvious
advantages to this approach: the programmer doesn't have to know anything about
writing HTML and we have a very abstract description of the GUI. In fact, it is
not tied to HTML at all: we could just as well generate a GUI for a desktop
application.

Using the formlets library (TODO cite) we can also generate forms in a generic
way, much in the same way as we generate the HTML. A formlet generates two
functions:

\begin{code}
runFormlet :: Formlet a -> (Maybe a -> X.Html, FormParser a)
\end{code}

The first function is used to generate the HTML for a form and it takes an
optional default value. The second function is to be used after the form is
submitted, and it tries to parse the HTTP request variables to construct a value
of type |a|. We can now generate forms for, for example, a User. 

However, if we want to apply the same trick and use a view similar to
|UserView|, we have a problem. We can generate the formlet, but the resulting
value is a |UserView|. Instead of just a function with type |User -> UserView|
we will also need a function that takes the original |User| and updates the new
values from |UserView|. This is where we can use updatable views. We have built
a prototype library for this, and in our research we want to see how we can
use this to generate abstract user interfaces. We want to investigate how we can
also customize layout while keeping our library abstract.

Previous work in this area has been done by Achten et al. \cite{ggui}. However,
their work is based on a manually written bijection. Our library is based on
lenses and provides combinators for writing the \emph{get} and \emph{putback}
function at the same time. We believe our approach is more powerful and safer.

\subsubsection{Generic database access}

Just like the views and forms above, we can generically derive methods to
persist entities on disk. Often this is done using a database. By writing a
number of small generic methods we can easily build a persistancy layer that
stores our model in a database. We have completed a prototype library that does
exactly this. In our research we want to explore this area further and see
whether we can apply more techniques from database programming.

\subsection{Control flow: a library for workflow control-flows}



1. What are workflows and control-flows?
In any complex system, the workflow needs to be encoded. Most of the time
this is done implicitly. iTasks \cite{iTasks} is a library that provides
first-class combinators for definining the workflow of a web-application. We
believe we can use a similar set of combinators for defining workflows for all
platforms supported by our framework.

2. What is the issue with HTTP?
However, there are some caveats when writing a control flow library.
Specifically, when using web-based applications, all clients are stateless. This
is something that has been studied extensively by others (TODO: citations). By
working with continuations, we can 

3. How can it be solved?

4. What is our solution?

As stated
before, the abstractions have great implications on the expressiveness of 

Write about the control flow library. Using right abstraction (arrows). Explain
why we're not using Monads. Talk about Hughes's paper. Serializing state.
Transform control flow structure into 

\subsection{Porting an existing application}


By porting an existing application written in PHP to our framework we want to
compare both approaches on a number of metrics. We are interesting in finding
out how the two applications compare on the following properties:

\begin{itemize}
\item Code size
\item Execution time
\item Expresiveness
\item Modifiability
\item Reliability
\item Portability
\end{itemize}

We expect to end up with an application with the same functionality, much better
performance (PHP is an interpreted language). Also, because Haskell allows for
more abstraction, we expect to end up with a lot less code, which is good for
modifiability. The typechecker will give more guarantees about our program and
thus is good for reliability.  Because of our declarative way of building
applications we expect that we can very easily port it to other architectures.


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
%\section{Research question}
%
%This thesis project will investigate what is needed to have a workflow modeling tool
%where the implementor can build workflows in a composable way using the language
%Haskell. We want to know: 
%
%\begin{itemize}
%\item How can we design a workflow modeling tool where a workflow specification is platform-agnostic?  
%\item How can we design that system in such a way that it has a type-safe core language?  
%\item How can we minimize application-specific code by using generic programming?
%\end{itemize}
%
%\subsection{Contribution}
%
%Compared to other approaches, we will deliver a solution that is strongly typed
%at the highest and lowest level. By using modern techniques like GADTs and
%type-level programming we can build libraries that are fully type-checked using
%the compiler. Also, we will build a number of libraries that can be used for
%building workflow applications.
%
%\section{Approach}
%
%We propose to build a workflow modeling tool in Haskell that provides the user
%with a way to express workflows in an implementation-agnostic way. We will port an
%existing workflow application that is written in PHP to show how our system
%compares against a manual implementation of a workflow application. 
%
%Using this application as a starting point, we will try to keep the
%application-specific code to a minimum by generalizing as much code as possible
%into libraries. 
%
%\subsection{The workflow library}
%\subsection{Generic Programming library}
%\subsection{Other libraries}
%
%\section{Expected results}
%
%We expect to end up with at least one application and two libraries. The first
%library will be for building interactive workflows. The other library will
%be for the generic programming on the domain model. It will provide facilities
%for writing your own generic function as well as a number of default functions.
%
%We suspect that the ported application will have less than half the LOC compared
%to the original version. PHP provides not nearly as much mechanisms for
%abstraction as Haskell. Also, we will end up with a fully type-checked version
%of the application. Security is no longer an issue if we take the right
%precautions at the type-level. Also, we expect the application to run much
%faster than its PHP variant because we will compile it using an optimizing
%compiler, whereas the PHP variant is interpreted.

\section{Planning}

We have made a timetable of the project , starting from the point where
the proposal is ready. During the research, we want to alternate between writing
and doing the research.

\begin{tabular}{l l}
Week  & Action \\
40    & Proposal ready \\
41    & Research: Identify common elements in database-driven applications \\
42    & Research: Identify generic functions to be written \\
43    & Research: Research power of lenses (also: compared to tools like
Interface Builder) \\
44    & Research: Writeup \\
45    & Research: Database layer (CRUD functions) \\
46 - 47  & Research: Database relations (one-to-many, etc.) \\
48 - 49   & Research: View abstractions (patterns recurring in the view layer)
\\
50    & Research: Business-logic abstractions (common patterns in the controller
layer) \\
51    &  \\
52 - 1 & Holiday \\
1     & Research: Business-logic abstractions \\
2 - 3 & Research: Workflow combinators \\
3 - 5 & Writeup \\
6 - 10 & Implementation: porting an existing application \\
10 - 15 & Writing \\
15    & Thesis ready for review \\
17    & Thesis defense \\
18    & Final corrections \\
\end{tabular}

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}

-- implementation issues
