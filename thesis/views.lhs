%if False

> {-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies, TypeOperators #-}
> module Views where

> import Generics.Regular
> import Generics.Regular.Views
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

\section{Generic programming}
\ref{sec:introgp}

\todo{What is generic programming?}

\section{Simple generic views}

In Haskell, we can easily generate an HTML representation of our datatype using
generic programming. For example, consider the |Person| datatype:

> data Person  = Person  { firstName :: String, lastName :: String, email :: String }

%if False

>   deriving Show

%endif

We can now use the regular library to derive a structural view on |Person|:

> $(deriveAll ''Person "PFPerson")
> type instance PF Person = PFPerson

These two lines make sure that |Person| is an instance of the |Regular|
typeclass. The type |PFPerson|, which is called the \emph{pattern functor}, 
describes the structure of the |Person| type in
terms of basic combinators such as the product and sum datatype.

We can write a function that operates on any structural view:

\begin{spec}
ghtml :: (Regular a, GHtml (PF a)) => a -> X.Html
\end{spec}

The implementation of the function |ghtml| is trivial, we refer to the regular-web
package for details. However, the functionality is interesting: we can generate
HTML for almost any datatype. The type signature is interesting, it contains two
type-class constraints: the type |a| has to be an instance of regular (which
means there are functions to convert |a| to and from its structural type |PF a|.
The other constraint is that |PF a| should be an instance of |GHtml|.

We can now generate generate the HTML for |chris|, an example value of the
|Person| datatype:

> chris = Person "chris" "eidhof" "chris@eidhof.nl"

If we if we print the value of |ghtml chris| we get the following result:

\begin{verbatim}
<h1>Person</h1 >
<label>FirstName: </label>chris<br />
<label>LastName: </label >eidhof<br />
<label>Email: </label>chris@eidhof.nl> 
\end{verbatim}

\section{Customization}
\label{sec:customization}

Generating HTML based on the structure of the type is a feature that virtually
every web framework has. It is very useful to quickly scaffold some basic pages.
However, most web applications require you to change the generated code if you
are not satisfied with it. This is problematic, because if your source type
changes, you either lose your changes (if you generate the code again) or you
have to manually adapt the changed code for the changes in the datatype.  

If we go back to the |Person| datatype, we might want to generate a different
piece of HTML for the email value. For example, we might want to leave out parts
of the email address or encode it as an image. So instead of generating that HTML
directly from the |Person| datatype, we add an indirection. We first
convert the |Person| datatype into a |PersonView| datatype, and generically
generate the HTML from that. This way, we can maintain a clean separation
between the model code and the view code and we alleviate the need of writing
HTML manually. First, consider the |PersonView| datatype:

> data PersonView = PersonView  {  lastName_PV   :: String
>                               ,  email_PV  :: Email}

We can now write a conversion function from |Person| to |PersonView|:

> personToPersonView (Person firstName lastName em) = 
>     PersonView lastName (Email em)

And using the same technique as above, we can generate |HTML| for values of the
|PersonView| datatype. This is powerful, because we can keep using the |Person|
datatype from our database, and only change it in the view part of the code.

\subsection{Intermezzo: model view controller}

Most web applications are structured using the Model, View, Controller paradigm.
We believe this is a good way to separate logical parts of an application: The
model part is concerned with the data storage, the view presents data to the end
user and the controller coordinates all of this.

The |Person| datatype that we have used as our running example is used for
generating view code. However, that datatype typically comes from our model. If
we change the order of fields or even the types of fields to generate better
view code, we are blurring the distinction between model and view code. Changing
the order of fields might be acceptable, but where do we draw the line? Using
specific view datatypes helps us to generate custom views without having to
change the model code. Therefore, it keeps a clean separation of concerns.

\section{Formlets}

We will now present the interface of the formlets  library \cite{CLWY08essence},
which we will later use to build generic forms.
The formlets library provides a composable way to write forms. An important notion is that a
form always consists of two parts: one function generates the HTML of a form, the other
parses the data sent by that form. When we build a form manually, we always have
to make sure that the field names match on both sides. Also, we have to display
errors if the submitted data contains errors. 

We first introduce a couple of basic functions defined in the formlets library.
All elements in the form are automatically named, so name clashes are impossible.
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

We can now build a form for |Person| values:

\begin{spec}
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

The second function is a parse function that tries to parse the |PostData|
and yields an |a| if it succeeds or an |Error| with helpful error messages if it
fails.

\subsection{Generic forms}

Writing a function to generically build a form based on a datatype is again
fairly trivial. We omit the implementation, but we give the type signature:

\begin{spec}
gform  ::  (Regular a, GFormlet (PF a), Functor m, Applicative m, Monad m) 
       =>  Maybe a -> XForm m a
\end{spec}

In section \ref{sec:customization}, we have seen how we can
generate custom |HTML| for a datatype. However, doing this for forms is a bit
problematic: we not only need to write a function to convert a function |to| from
|Person| to |PersonView|, we also need a function |from| to go back from a |PersonView| to
|Person|. 

We can write the |to| and |from| functions by hand, but instead we will use the
|fclabels| package. It provides combinators for writing bi-directional
functions. In our example, we can not write a function with type |PersonView -> Person|, 
because we have lost information going from a |Person| to a
|PersonView|. Therefore, the only thing we can do is update the original
|Person| value with the new values from |PersonView|. In general, this is the
case, hence the type of the |set| function below.

The |fclabels| package gives us the type constructor |:->|. Using values of that type
, we can use the |get| and |set| functions, which have the following
types:

\begin{spec}
get :: (f :-> a) -> f -> a
set :: (f :-> a) -> a -> f -> f
\end{spec}

So if we create a value of type |Person :-> PersonView|, we can create a getter
and setter from that.

%if False

> $(mkLabels [''Person])

%endif

> personView :: Person :-> PersonView
> personView = Label (PersonView  <$>  lastName_PV   `for` lLastName 
>                                 <*>  email_PV      `for` lEmail')

Because |Email| is defined as a newtype, we need a helper function that converts
the |Person|s email value of type |String| to a value of type |Email|.

> lEmail' :: Person :-> Email
> lEmail' = (label Email (const . showEmail)) . lEmail

Note that we have used the functions |lEmail| and |lLastName|. They have been
generated by a Template Haskell from the |fclabels| package and have the
following types:

\begin{spec}
lEmail    :: Person :-> String
lLastName :: Person :-> String
\end{spec}

The Template Haskell code generates such a function for each record field in a
datatype. We now have all the pieces needed for a form function that uses the
|:->| datatype:

\begin{spec}
projectedForm :: (Regular a, GFormlet (PF a), Applicative m, Monad m) 
              => (b :-> a) -> b -> XForm m b
projectedForm proj x = (flip (set proj) x) <$> (gform (get proj <$> (Just x)))
\end{spec}

The |projectedForm| first converts the value |x| to its projected datatype, then
generates a generic form for it. It also changes the parser function such that
the original value |x| is returned with the changes from the form.

\section{Generating JSON and XML}

%if not thesis
\end{document}
%endif
