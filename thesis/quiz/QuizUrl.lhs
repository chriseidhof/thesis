%if False

> {-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls,
>              TypeSynonymInstances, TypeOperators, ScopedTypeVariables #-}
>
> module QuizUrl where
>
> import Generics.Regular
> import Data.Char (toLower)
> import Data.List (intercalate)
> import Routing
> import qualified Text.XHtml.Strict as X

%endif

Our web application responds to URLs. The URL forms part of the user
interface: they should be readable most of the times, and users should be able
to copy and paste them.
We build a datatype describing all valid
URLs in our application: users can list all the quizes, add a new quiz, view
a quiz and take a quiz.
As we see later, we give a handle for each constructor, describing the
corresponding functionality.
When we generate URLs from this datatype, the compiler guarantees that we only
produce valid URLs:

> data QuizRoute  =  List
>                 |  Add
>                 |  View  Int
>                 |  Take  Int
>   deriving Show

The datatype allows us to construct URLs from values of |QuizRoute| and parse URLs to
values of |QuizRoute| if we make it an instance of the  |ToURL| typeclass. The
typeclass captures that we can convert between the string representation of URLs and values of |QuizRoute|.
The user of the library gives the instance manually, for maximum flexibility.
However, often this flexibility is not needed, and the instance can be given in
terms of |gtoURL| and |gfromURL|.

> instance ToURL QuizRoute where
>  toURL    = gtoURL  . from
>  fromURL  = fmap to . gfromURL

The functions |gtoURL| and |gfromURL| are generic functions and work on almost any datatype. These particular functions are defined using the 
regular\footnote{\url{http://hackage.haskell.org/package/regular}} library and can be found in the code accompanying this thesis.

%if False

In order for the generic functions |gtoURL| and |gfromURL| to work, we make |QuizRoute| an instance of the |Regular|
class, which is provided by the
regular\footnote{\url{http://hackage.haskell.org/package/regular}} library.
We do this using a Template Haskell call.
Because the Template Haskell in the regular library is designed to work with
an older version of Template Haskell that does not support type families,
we also manually give a type family instance:

> $(deriveAll ''QuizRoute "PFQuizRoute")
> type instance PF QuizRoute = PFQuizRoute

%endif

Finally, we provide a helper function that links to a |QuizRoute|. We use
this function to generate HTML that links to an action. Because the first
argument is a |QuizRoute| value, we can only link to correct URLs. We use the
\texttt{Text.XHtml.Strict} library for building HTML, which is imported with a
|qualified import| in the |X| namespace.

> static :: QuizRoute -> String -> X.Html
> static u s = X.anchor X.! [X.href $ "/" ++ renderURL (toURL u)] X.<< X.toHtml s
