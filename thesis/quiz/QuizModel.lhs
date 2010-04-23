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

We define our data model as an Entity Relationship model \cite{chen1976entity}, or \emph{ER model}\more{chap:ermodels}, a formalism for building data
models. In figure \ref{fig:quizermodel} you can see the graphical representation.
Each quiz has a subject and a longer description, and consists of a sequence of
questions. Each question belongs to exactly one quiz.
A response to a quiz contains the name and email-address of the respondant, as well as
the date on which the quiz was taken, and the list of answers. A quiz has many
responses, but every response belongs to a single quiz.

\begin{figure}
\includegraphics[width=15cm]{quiz/ermodel}
\caption{The data model for our quiz system.}
\label{fig:quizermodel}
\end{figure}

All the entities \more{sec:ermodels} (represented by rectangles) are encoded as Haskell datatypes,
with a record field for each attribute (represented by ovals).
In chapter \ref{chap:ermodels} we explain in detail how to encode an ER model in Haskell.

> data Quiz      = Quiz      {  subject :: String, description :: String}
> data Question  = Question  {  title :: String, choiceA, choiceB, choiceC :: String}
> data Response  = Response  {  name     :: String
>                            ,  email    :: Email
>                            ,  date     :: Date
>                            ,  answers  :: [Answer] 
>                            }

> data Answer    = QA | QB | QC deriving (Show, Eq, Enum)

We enumerate the entities of a |QuizModel| as a type-level list:
\more{sec:hlist}.

> type QuizModel = Quiz B.:*: Question B.:*: Response B.:*: Nil

%if False

> $(mkIndexes ''QuizModel  [''Quiz, ''Question, ''Response])

%endif

For both relationships (the diamonds in the figure) we add a type. Note that the
in the diagram, there is a |*| and a |1|
annotation on the lines connecting the relationship with its entities. This is
called the \emph{cardinality} of the relationship: \more{sec:cardinality} every |Question| entity belongs to a single |Quiz|
entity and every |Quiz| entity has several |Question| entities. We encode
this using the |One| and |Many| types, respectively.

> type Questions  = Rel  QuizModel  One   Quiz  Many  Question
> type Responses  = Rel  QuizModel  One   Quiz  Many  Response

We also provide information at the value level for both relationships:
\more{sec:encoding}

> questions   :: Questions
> questions  = Rel  One   ixQuiz  "quiz"  Many  ixQuestion  "questions"
>
> responses   :: Responses
> responses  = Rel  One   ixQuiz  "quiz"  Many  ixResponse "responses"

We also enumerate the relations at the type level:

> type QuizRelations = Questions B.:*: Responses B.:*: Nil

The rest of the code is very mechanical and is included at the end of this chapter, where we have listed the entire source code of this example.

%if False

Now we can make |QuizModel| and |QuizRelations| an instance of the |ERModel|
class. We need to provide some information on the value level, which is
explained in more detail in chapter \ref{chap:ermodels}. We plan to write
Template Haskell code for this, because the instance declaration below can be
constructed mechanically.

> instance ERModel QuizModel QuizRelations where
>   relations  =  TCons4 questions
>              $  TCons4 responses
>              $  TNil4 
>   witnesses  =  WCons ixQuiz $  WCons ixQuestion $  WCons ixResponse $  WNil

Finally, we provide indexes into the type-level lists for all entities 
. These are constructed using Template Haskell, and serve as proof that a
given entity is in the list of entities described by the ER model.

\begin{spec}
ixQuiz      :: Ix QuizModel Quiz
ixQuestion  :: Ix QuizModel Question
ixResponse  :: Ix QuizModel Response
\end{spec}

The following Template Haskell code generates the indexes above:

> $(mkIndexes ''QuizRelations [''Questions, ''Responses])

We also provide indexes for relationships, which point into the type-level lists
containing all relationships.

\begin{spec}
ixQuestions  :: Ix QuizRelations Questions
ixResponses  :: Ix QuizRelations Responses
\end{spec}

> $(deriveAll ''Quiz  "PFQuiz")
> $(deriveAll ''Question  "PFQuestion")
> $(deriveAll ''Response  "PFResponse")

> type instance PF Quiz = PFQuiz
> type instance PF Question = PFQuestion
> type instance PF Response = PFResponse

> $(mkLabels [''Quiz, ''Question, ''Response])

> $(mkEqualities [''Quiz, ''Question, ''Response])

%endif
