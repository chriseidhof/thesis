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

For every component (e.g., virtual URL directory) we define a datatype. Here's
the part for the |User| component.

> data QuizRoute = List
>                | Add
>                | View Int
>                | Take Int
>   deriving Show
>

This is some TH code which is necessary for using the Regular library:

> $(deriveAll ''QuizRoute "PFQuizRoute")
> type instance PF QuizRoute = PFQuizRoute

> instance ToURL QuizRoute where
>  toURL = gtoURL . from
>  fromURL = fmap to . gfromURL

> static :: QuizRoute -> String -> X.Html
> static u s = X.anchor X.! [X.href $ "/" ++ renderURL (toURL u)] X.<< X.toHtml s
