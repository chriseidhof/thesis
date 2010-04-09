> {-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeSynonymInstances,
>     TypeFamilies, FlexibleContexts, RankNTypes, Arrows, TemplateHaskell,
>     EmptyDataDecls
>   #-}

> module Quiz where

> import Basil hiding ((:*:))
> import qualified Basil as B
> import ArrowBased
> import qualified Data.Set as S
> import qualified Control.Monad.State as ST
> import Generics.Regular
> import Generics.Regular.Formlets
> import Control.Concurrent.MVar

> newtype Email = Email { unEmail :: String }
> newtype Date  = Date  { unDate  :: String }

To build a |Quiz| application, we first define its data model.

> data Quiz      = Quiz {subject :: String, description :: String}
> data Question  = Question {title :: String, choiceA, choiceB, choiceC :: String}
> data Response  = Response { name :: String
>                           , email :: Email
>                           , date :: Date
>                           , answers :: [Answer] }
> data Answer    = A | B | C

> $(deriveAll ''Quiz  "PFQuiz")
> $(deriveAll ''Question  "PFQuestion")
> $(deriveAll ''Response  "PFResponse")

> type instance PF Quiz = PFQuiz
> type instance PF Question = PFQuestion
> type instance PF Response = PFResponse

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


> type M a = Basil QuizModel QuizRelations a
> type St = BasilState QuizModel QuizRelations
> type Web i o = WebT IO i o

> instance DefaultForm Quiz     where form = gform Nothing
> instance DefaultForm Question where form = gform Nothing

> basil :: (a -> M b) -> Web (a, MVar St) b
> basil f = liftAM $ \(a, refState) -> do
>             st <- takeMVar refState
>             let (x, st') = contBasil (f a) st
>             putMVar refState st'
>             return x

> addQuiz :: Web (MVar St) (Ref QuizModel Quiz)
> addQuiz = proc st -> do
>   q   <- input -< ()
>   (basil (\x -> new ixQuiz x PNil)) -< (q, st)

> addQuestions = proc (st, q) -> do
>   qs  <- inputMany -< ()
>   basil (\(qs,qz) -> mapM (\question -> new ixQuestion question (PCons qz PNil)) qs) -< ((qs, q), st) 

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
