%if False

> {-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies #-}
> module Views where

> import Generics.Regular
> import Generics.Regular.Views


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

If we calculate the value of |ghtml chris| we get the following result:

\begin{verbatim}
<h1>Person</h1 >
<label>FirstName: </label>chris<br />
<label>LastName: </label >eidhof<br />
<label>Email: </label>chris@eidhof.nl> 
\end{verbatim}

\section{Customization}

Generating HTML based on the structure of the type is a feature that virtually
every web framework has. It is very useful to quickly scaffold some basic pages.
However, when you want to do more advanced things, you quickly have to build
your own HTML. In this section, we should how we can prevent that.

If we go back to the |Person| datatype, we might want to generate a different
piece of HTML for the email value. For example, we might want to leave out parts
of the address or encode it as an image. So instead of generating that HTML
directly from the |Person| datatype, we add another indirection. We first
convert the |Person| datatype into a |PersonView| datatype, and generically
generate the HTML from that. This way, we can maintain a clean separation
between the model code and the view code and we alleviate the need of writing
HTML manually.

TODO: show an example.

Back and forth: using lenses (fclabels) to apply the same technique to forms.

%if not thesis
\end{document}
%endif
