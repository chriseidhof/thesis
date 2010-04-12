%if False

> {-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies, TypeOperators #-}
> module Views where

> import Generics.Regular
> import Generics.Regular.Views
> import Generics.Regular.Formlets
> import Control.Applicative
> import Data.Record.Label
> import Prelude hiding ((.))
> import Control.Category

> newtype Email = Email {showEmail :: String} deriving Show

> 


%endif
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
\title{Generic views for web programming}

\maketitle

\tableofcontents

%endif

\section{Introduction}

Most web applications are structured using the Model, View, Controller paradigm.
We believe this is a good way to separate logical parts of an application: The
model part is concerned with the data storage, the view presents data to the end
user and the controller coordinates all of this.

The view code is often heavily dependent on model code, and as a result,
most web programming frameworks conveniently include features to build view code
based on the structure of the model.
For example, Ruby on Rails can generate HTML templates
for each model class, and the generated templates can be edited by hand to have
maximum control over the output.
However, this approach is problematic: \emph{if the model changes, either the template has to be
changed by hand, introducing a possibility for errors, or the template has to be
generated again, losing the changes made so far}.

Generating code can save a lot of time when initially creating an application,
but can introduce a maintenance problem if the generation source changes.
While code generation can reduce program size and program complexity, developers
refrain from using it because 

\todo{explain more about why it is important.}

We use generic programming \todo{citation} as a technique for generating view
code based on the structure of the model. However, generic programming suffers
from a different problem: \emph{it is not possible to change the structure of a
generic programming, except by changing the structure of the input}. 
Changing our model code to accomodate for the view code clearly breaks
abstractions and is not the right solution to this problem. 
Not using generic programming is one solution, but implies that we need to write
a lot more code.

The solution we present in this chapter combines bidirectional programming with generic
programming. A bidirectional program works in two ways: if a program converts from
|A| to |B|, there is also a way to go back from a |B| to an |A|. The fclabels
library\footnote{\url{http://hackage.haskell.org/package/fclabels}} 
provides combinators to construct \emph{lenses} in Haskell, which allow
for bidirectional programming. A lens is defined as a pair of functions |a -> c|
and |c -> a -> a|. Suppose we have a model datatype |M| and a generic function
that converts |M| into HTML. To change the output of the generic function we add a view datatype
|V| which has the right structure, and build a lens from |M| to |V|. Now we can
first convert an |M| into a |V| value and use the generic function on the |V|
value.

In section \ref{sec:ghtml} we will describe how to generate HTML using generic
programming.
Section \ref{sec:gform} describes how to build type-safe forms based on the
formlets\footnote{\url{http://hackage.haskell.org/package/formlets}} library
\cite{formlets}, and in section \ref{sec:gjson} we show how we can derive an
API from our model. In the last sections we describe future work and
conclude. The functions described in this section are released as part of the regular-web 
package\footnote{\url{http://hackage.haskell.org/package/regular-web}}.

\section{Generic HTML generation}
\label{sec:ghtml}

In Haskell, we can easily generate an HTML representation of our datatype using
generic programming. For example, consider the |Person| datatype:

> data Person  = Person  { name :: String, birthDate :: Date, email :: Email }

%if False

>   deriving Show

> newtype Date  = Date {unDate :: String}
>  deriving Show

> obfuscate = undefined

%endif

We can now use Template Haskell code from the the regular library \todo{cite} to derive a structural view on |Person|:

> $(deriveAll ''Person "PFPerson")
> type instance PF Person = PFPerson

The two Template Haskell calls make sure that |Person| is an instance of the |Regular|
typeclass. The type |PFPerson|, which is called the \emph{pattern functor}, 
describes the structure of the |Person| type in
terms of basic combinators such as the product and sum datatype.

We will use a function |ghtml| that generates HTML for any |a| that can be
converted into a structural view:

\begin{spec}
ghtml :: (Regular a, GHtml (PF a)) => a -> X.Html
\end{spec}

The implementation of the function |ghtml| is trivial, we refer to the regular-web
package for details.
However, the functionality is interesting: we can generate
HTML for almost any datatype |a|. The type contains two
type-class constraints: first, the type |a| has to be an instance of the |Regular|
type-class, which means there are functions to convert between |a| and its structural type |PF a|.
Second, the structural type |PF a| has to be an instance of |GHtml|.

As an example,  can generate the HTML for |chris|, an example value of the
|Person| datatype:

> chris = Person "chris" (Date "16-01-85") (Email "chris@eidhof.nl")

If we if we print the value of |ghtml chris| we get the following result:

\begin{verbatim}
<h1>Person</h1 >
<label>Name: </label>chris<br />
<label>BirthDate: </label >16-01-85<br />
<label>Email: </label>chris@eidhof.nl> 
\end{verbatim}

\subsection{Customization}
\label{sec:customization}

Generating HTML based on the structure of the type is a feature that virtually
every web framework provides.
It is very useful to quickly set up some basic pages, however: most web applications require you to change the generated code if you
are not satisfied with it.
As explained in the introduction, this is problematic, because if the source
type changes, you either have to manually update the changes in the view code,
or lose the customizations.

Suppose we want to customize the way HTML is generated for the |Person|
datatype: we do not want to show the birthDay and we only show the first part of
someone's e-mail address.
Instead of generating that HTML
directly from the |Person| datatype, we add an indirection: we first
convert the |Person| datatype into a |PersonView| datatype, and generically
generate the HTML for the |PersonView| value.
This way, we can maintain a clean separation
between the model code and the view code without having to write manual HTML.
First, we define the |PersonView| datatype:

> data PersonView = PersonView  {  _name   :: String
>                               ,  _email  :: String 
>                               }

We can now write a conversion function from |Person| to |PersonView|:

> personToPersonView (Person name bDate em) = 
>     PersonView name (obfuscate em)

And using the same technique as above, we can generate |HTML| for values of the |PersonView| datatype.
This is powerful, because we can keep using the |Person| datatype from our
database, and only change it in the view part of the code. We have maintained
the separation of concerns while having full flexibility over the view.

\section{Generic Forms}
\label{sec:gform}

In this section, we will build type-safe, composable, error-checking forms using the formlets
library \cite{formlets}. A form with type |Form a| tries to parse values of |a|.
Its input is automatically checked for mistakes and a user-friendly
error-message is shown if a mistake was made. We will first introduce the
formlets library and then show how to build formlets generically.
Finally, to change the structure of the forms we will use bi-directional
programming.

The formlets library provides combinators to build forms. An important notion is that a
form always consists of two parts: one function generates the HTML of a form, the other
parses the data sent by that form. When we build a form manually, we always have
to make sure that the field names in the HTML match the field names in the
parser function. Using formlets, the HTML and the parser function are constructed
at the same time so that field names will always match.

We first introduce some basic functions defined in the formlets library.
The |input| function produces a form with just an input field:

\begin{spec}
input :: Form String
\end{spec}

The |<*>| operator combines two forms. The first form is of type |a -> b|, which
means that when it is combined with a |Form a|, it will yield a |Form b|:

\begin{spec}
(<*>) :: Form (a -> b) -> Form a -> Form b
\end{spec}

The |check| function is used for the parsing. It first gets a function that
checks whether the data is in the correct format. The type |Either Error b| captures
that it is either an error message or a  value of type |b| (which means the
value was succesfully parsed). 

\begin{spec}
check :: (a -> Either Error b) -> Form a -> Form b
\end{spec}

Finally, whenever we want to lift functions or other values to the type level
we can use the |pure| function:

\begin{spec}
pure  :: a -> Form a
\end{spec}

In our standard library we also have functions that check whether a date is
entered correctly or an email-address is in the right format:

\begin{spec}
checkDate   :: String -> Either Error Date
checkEmail  :: String -> Either Error Email
\end{spec}

We can now build a form for |User| values:

\begin{spec}
data Person = User {name :: String, birthDate :: Date, email :: Email}

personForm :: Form Person
personForm  =    pure Person 
            <*>  input 
            <*>  check checkDate   input 
            <*>  check checkEmail  input
\end{spec}

The formlets library provides functions to get the |HTML| of a form, and the
parsing function. The simplified type signature is similar to:

\begin{spec}
runForm :: Form a -> (HTML, PostData -> Either Error a)
\end{spec}

The second function is a parse function that parses the |PostData|
and yields an |a| if parsing succeeds, or an |Error| if parsing fails. The
|Error| type contains a list of error messages.

\subsection{Generic forms}

Using the combinators above, we can write a generic function that produces a
form for any type |a|.  We omit the implementation, but we give the type signature:

\begin{spec}
gform  ::  (Regular a, GFormlet (PF a)) 
       =>  Maybe a -> Form a
\end{spec}

In section \ref{sec:customization}, we have seen how we can
generate custom |HTML| for a datatype. However, we have to use a different
technique for forms. Not only do we have to change the input of a form (a value
with type |Maybe a|), we also have to change the output of a form. For example,
consider again the |Person| from the previous section. If we want to change the
form or a |Person|, we can introduce a |PersonForm| datatype:

> data PersonForm = PersonForm { __name :: String, __birthDate :: Date}

However, if we specialzie the function |gform| for |PersonForm|, we get the
following type:

\begin{spec}
pform  :: Maybe PersonForm -> Form PersonForm
\end{spec}

If we write a function |personToPersonForm|, we can change |pForm| to take a
|Maybe Person| as input, but the output will remain the same. This is where
bidirectional programming can help us. Using the lenses library, we can write a
lens between |Person| and |PersonForm|, and convert the input of type |Person|
to a |PersonForm|, and use the form output of type |PersonForm| to update the
original input, yielding a changed |Person| value.

\begin{spec}
projectedForm :: (Regular a, GFormlet (PF a)) => (b :-> a) -> b -> Form m b	
\end{spec}

Note howe the input and output of the form are of type |b|, but the form is
rendered using type |a|. The first argument, a lens of type |b :-> a|, is used
to convert between |b| and |a| values.
We can now proceed do define such a lens. For a detailed explanation of how to
construct lenses using the fclabels package, see \todo{TR fclabels}.

%if False

> $(mkLabels [''Person])

> $(deriveAll ''PersonForm "PFPersonForm")
> type instance PF PersonForm  = PFPersonForm

> instance Formlet Date

%endif

> convertPerson :: Person :-> PersonForm
> convertPerson = Lens  (PersonForm  <$>  __name       `for` lName 
>                                    <*>  __birthDate  `for` lBirthDate)

If we apply |projectedForm| to the lens |convertPerson|, we get the following
function:

\begin{spec}
personForm :: Person -> Form Person
personForm = projectedForm convertPerson
\end{spec}

The |projectedForm| first converts the value |x| to its projected datatype
|PersonForm|, then generates a generic form for it.
It also changes the parser
function such that the original value |x| is returned with the changes from the
form.
Using lenses allowed us to construct the necessary bidirectional function using
simple combinators.
We have again modified our view code without changing our model code or
constructing forms manually.

\section{Generating JSON and XML}
\label{sec:gjson}

%What is the problem?
Web application developers sometimes build an application programming interface,
or \emph{API}.
This allows other developers to access and/or modify the data of the
application.

%Why does it matter?
Constructing such an API manually is tedious work, and as with views, an API
often largely reflects the structure of the model.

% What is our solution?
Use generic programming, we can derive an API automatically from our model
datatypes.
However, if we have other programs that access our API, it is important that our
API is stable: the structure should not change very often.
If we use generic programming, every change in our model will be reflected in
the API.
We show that by using our lenses library we can solve this problem.

\subsection{Generic JSON generation}

Our library provides the functions |toJSON| and |fromJSON|. 

\section{Future work}

Statically typed APIs a la Diatchki.

\section{Conclusion}

\section{Library interface}

TODO.

%if not thesis

\bibliographystyle{plain}

\bibliography{bibliography}


\end{document}
%endif
