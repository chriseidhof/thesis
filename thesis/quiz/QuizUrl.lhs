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

Our web application responds to URLs. We believe the URL forms part of the user
interface: they should be readable most of the times, and users should be able
to copy and paste them.
We build a datatype for all possible
URLs in our application: The user can list all the quizes, add a new quiz, view
a quiz and take a quiz.
If we generate URLs using this datatype, the compiler verifies that we only produce correct URLs.

> data QuizRoute  =  List
>                 |  Add
>                 |  View  Int
>                 |  Take  Int
>   deriving Show

The datatype allows us to construct URLs from values of |QuizRoute| and parse URLs to
values of |QuizRoute| if we make it an instance of the  |ToURL| typeclass. The
typeclass captures that we can convert between the string representation of URLs and values of |QuizRoute|.
The |toURL| and |fromURL| functions are defined using the |gtoURL| and
|gfromURL| functions, which are generic functions.

> instance ToURL QuizRoute where
>  toURL    = gtoURL . from
>  fromURL  = fmap to . gfromURL

In order for the generic functions to work, we need to make |QuizRoute| an instance of the |Regular|
class, a generic programming library. This is done using a Template Haskell call
defined in the regular\footnote{\url{http://hackage.haskell.org/package/regular}} library.

> $(deriveAll ''QuizRoute "PFQuizRoute")
> type instance PF QuizRoute = PFQuizRoute

Finally, we provide a helper function that links to a |QuizRoute|. We use
this function to generate HTML that links to an action. Because the first
argument is a |QuizRoute| value, we can only link to correct URLs.

> static :: QuizRoute -> String -> X.Html
> static u s = X.anchor X.! [X.href $ "/" ++ renderURL (toURL u)] X.<< X.toHtml s
