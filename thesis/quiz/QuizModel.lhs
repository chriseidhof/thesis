%if False

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

%endif

We define our data model as an ER model, which is a formalism for building data
models. In figure \ref{fig:quizermodel} you can see its graphical representation.
Every quiz has a subject and a longer description, and can have many questions,
but every question belongs to exactly one quiz.
A response to a quiz contains the name and email-address of the respondant, as well as
the date on which the quiz was taken, and a list of answers. A quiz has many
responses, but every response belongs to exactly one quiz.

\begin{figure}
\includegraphics[width=15cm]{quiz/ermodel}
\caption{The data model for our quiz system.}
\label{fig:quizermodel}
\end{figure}

All the entities (represented by rectangles) are encoded as Haskell datatypes,
with a record field for each attribute (represented by ovals).

> data Quiz      = Quiz      { subject :: String, description :: String}
> data Question  = Question  { title :: String, choiceA, choiceB, choiceC :: String}
> data Response  = Response  { name     :: String
>                            , email    :: Email
>                            , date     :: Date
>                            , answers  :: [Answer] 
>                            }

> data Answer    = QA | QB | QC deriving (Show, Eq, Enum)

We also enumerate the entities of a |QuizModel| in a type-level list.

> type QuizModel = Quiz B.:*: Question B.:*: Response B.:*: Nil

For both relationships, defined by the diamonds, we add a type. Note that the
relationships have a cardinality. In the diagram, there is a |*| and a |1|
annotation on the lines connecting the relationship with its entities. We encode
this using the |Many| and |One| types, respectively.

> type TQuestions  = Rel  QuizModel  One   Quiz  Many  Question
> type TResponses  = Rel  QuizModel  One   Quiz  Many  Response

We also provide information on the value level for both relations:

> questions   :: TQuestions
> questions  = Rel  One   ixQuiz  "quiz"  Many  ixQuestion  ""
>
> responses   :: TResponses
> responses  = Rel  One   ixQuiz  "quiz"  Many  ixResponse "responses"

And we enumerate the relations in a type-level list:

> type QuizRelations = TQuestions B.:*: TResponses B.:*: Nil

Now we can make |QuizModel| and |QuizRelations| an instance of the |ERModel|
class. We need to provide some information on the value level, which is
explained in more detail in chapter \ref{chap:ermodels}.

> instance ERModel QuizModel QuizRelations where
>   relations  =  TCons4 questions
>              $  TCons4 responses
>              $  TNil4 
>   witnesses  =  WCons ixQuiz $  WCons ixQuestion $  WCons ixResponse $  WNil

Finally, we provide indexes into the type-level lists for all entities and
relationships. These are constructed mechanically, and serve as proof that a
given entity is in the list of entities described by the ER model.

> ixQuiz     :: Ix QuizModel Quiz
> ixQuestion :: Ix QuizModel Question
> ixResponse :: Ix QuizModel Response

> ixQuestions :: Ix QuizRelations TQuestions
> ixResponses :: Ix QuizRelations TResponses

%if False


> ixQuiz = Zero
> ixQuestion = Suc Zero
> ixResponse = Suc (Suc Zero)

> ixQuestions = Zero
> ixResponses = Suc Zero

> $(deriveAll ''Quiz  "PFQuiz")
> $(deriveAll ''Question  "PFQuestion")
> $(deriveAll ''Response  "PFResponse")

> type instance PF Quiz = PFQuiz
> type instance PF Question = PFQuestion
> type instance PF Response = PFResponse

> $(mkLabels [''Quiz, ''Question, ''Response])

> type instance TypeEq Quiz     Quiz     = True 
> type instance TypeEq Quiz     Question = False
> type instance TypeEq Quiz     Response = False
> type instance TypeEq Question Quiz     = False
> type instance TypeEq Question Question = True
> type instance TypeEq Question Response = False
> type instance TypeEq Response Quiz     = False
> type instance TypeEq Response Question = False
> type instance TypeEq Response Response = True

%endif
