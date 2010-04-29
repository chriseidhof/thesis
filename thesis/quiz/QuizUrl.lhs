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

Every web application responds to URLs. The URL forms part of the user
interface: they should be readable most of the times, and users should be able
to copy and paste them.
We build a datatype from which we generate all valid
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

This datatype allows us to construct URLs from values of |QuizRoute| and parse URLs to
values of |QuizRoute|, provided we make it an instance of the  |ToURL|
type class. That
type class captures that we can convert between the string representation of URLs and values of |QuizRoute|.
The user of the library gives the instance explicitly, but since this is most
often done in the same way, we can give default definations using the functions
|gtoURL| and |gfromURL|:

> instance ToURL QuizRoute where
>  toURL    = gtoURL  . from
>  fromURL  = fmap to . gfromURL

The functions |gtoURL| and |gfromURL| are generic functions and work on almost any datatype. These particular functions are defined using the 
\emph{regular} \cite{regular} library and can be found in the code accompanying this thesis.

%if False

> $(deriveAll ''QuizRoute "PFQuizRoute")
> type instance PF QuizRoute = PFQuizRoute

%endif

Finally, we provide a helper function |static| that produces a hyperlink to a |QuizRoute|
URL.
Because the first
argument is a |QuizRoute| value, we can only link to correct URLs. We use the
\texttt{Text.XHtml.Strict} library for building HTML, which is imported with a
qualified import in the |X| namespace.

> static :: QuizRoute -> String -> X.Html
> static u s = X.anchor X.! [X.href $ "/" ++ renderURL (toURL u)] X.<< X.toHtml s
