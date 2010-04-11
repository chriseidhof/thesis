> {-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeSynonymInstances,
>       TypeFamilies, FlexibleContexts, RankNTypes, Arrows, TemplateHaskell, EmptyDataDecls, FlexibleInstances
>  #-}

> module QuizModel where

> import Data.Record.Label
> import Generics.Regular
> import Generics.Regular.Views
> import Generics.Regular.Formlets
> import Control.Applicative
> import Basil hiding ((:*:))
> import qualified Basil as B

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

> type instance TypeEq Quiz     Quiz     = True 
> type instance TypeEq Quiz     Question = False
> type instance TypeEq Quiz     Response = False
> type instance TypeEq Question Quiz     = False
> type instance TypeEq Question Question = True
> type instance TypeEq Question Response = False
> type instance TypeEq Response Quiz     = False
> type instance TypeEq Response Question = False
> type instance TypeEq Response Response = True

> questions  = Rel  One   ixQuiz  "quiz"  Many  ixQuestion  ""
> responses  = Rel  One   ixQuiz  "quiz"  Many  ixResponse "responses"
> ixQuestions = Zero
> ixResponses = Suc Zero

> ixQuiz = Zero
> ixQuestion = Suc Zero
> ixResponse = Suc (Suc Zero)

> type QuizModel = Quiz B.:*: Question B.:*: Response B.:*: Nil


> type TQuestions  = Rel  QuizModel  One   Quiz  Many  Question
> type TResponses  = Rel  QuizModel  One   Quiz  Many  Response
> questions   :: TQuestions
> responses   :: TResponses

> type QuizRelations = TQuestions B.:*: TResponses B.:*: Nil

> ixQuiz     :: Ix QuizModel Quiz
> ixQuestion :: Ix QuizModel Question
> ixResponse :: Ix QuizModel Response

> ixQuestions :: Ix QuizRelations TQuestions
> ixResponses :: Ix QuizRelations TResponses

> instance ERModel QuizModel QuizRelations where
>   relations  =  TCons4 questions
>              $  TCons4 responses
>              $  TNil4 
>   witnesses  =  WCons ixQuiz $  WCons ixQuestion $  WCons ixResponse $  WNil

