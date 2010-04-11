> {-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeSynonymInstances,
>       TypeFamilies, FlexibleContexts, RankNTypes, Arrows, TemplateHaskell, EmptyDataDecls, FlexibleInstances
>  #-}

> module QuizModel where

> import Data.Record.Label
> import Generics.Regular
> import Generics.Regular.Views
> import Generics.Regular.Formlets
> import Control.Applicative

> newtype Email = Email { unEmail :: String }
> newtype Date  = Date  { unDate  :: String }

> instance Html Email where html = html . unEmail
> instance Html Date where html = html . unDate
> instance Html [Answer] where html = html . concatMap show

> instance Formlet Email where formlet x = Email <$> formlet (unEmail <$> x)

> data Quiz      = Quiz {subject :: String, description :: String}
> data Question  = Question {title :: String, choiceA, choiceB, choiceC :: String}
> data Response  = Response { name :: String
>                           , email :: Email
>                           , date :: Date
>                           , answers :: [Answer] }

> data Answer    = QA | QB | QC deriving (Show, Eq, Enum)

> $(deriveAll ''Quiz  "PFQuiz")
> $(deriveAll ''Question  "PFQuestion")
> $(deriveAll ''Response  "PFResponse")
> $(mkLabels [''Quiz, ''Question, ''Response])

> type instance PF Quiz = PFQuiz
> type instance PF Question = PFQuestion
> type instance PF Response = PFResponse


