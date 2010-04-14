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

We define a datatype with all the possible actions in our library.
The user can list all the quizes, add a new quiz, view a quiz and take a quiz.

> data QuizRoute = List
>                | Add
>                | View Int
>                | Take Int
>   deriving Show
>

Using Template Haskell, we can make |QuizRoute| an instance of the |Regular|
class, a generic programming library.

> $(deriveAll ''QuizRoute "PFQuizRoute")
> type instance PF QuizRoute = PFQuizRoute

This allows us to construct URLs from values of |QuizRoute| and parse URLs to
values of |QuizRoute|:

> instance ToURL QuizRoute where
>  toURL = gtoURL . from
>  fromURL = fmap to . gfromURL

Finally, we provide a helper function that links to a |QuizRoute|. Using this
function, we can link to other actions in our page without constructing URLs
as |String| values. This gives us the guarantee that we only construct
well-formed and valid URLs.

> static :: QuizRoute -> String -> X.Html
> static u s = X.anchor X.! [X.href $ "/" ++ renderURL (toURL u)] X.<< X.toHtml s
