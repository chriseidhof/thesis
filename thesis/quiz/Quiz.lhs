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
> import Generics.Regular.Views
> import Data.Record.Label
> import Control.Applicative hiding (many)
> import Control.Concurrent.MVar
> import Control.Arrow
> import qualified Text.XHtml.Strict as X
> import qualified Data.Map as M
> import Data.List (intersperse)
> import Data.Maybe (catMaybes)
> import QuizUrl
> import QuizModel
> import qualified Text.XHtml.Strict.Formlets as F


To build a |Quiz| application, we first define its data model.

> data ResponseView  = ResponseView { _name :: String
>                                   , _email :: Email
>                                   }

> $(deriveAll ''ResponseView  "PFResponseView")
> type instance PF ResponseView = PFResponseView

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
> type Web i o = WebT (MVar St) IO i o

> instance DefaultForm Quiz     where form = gform Nothing
> instance DefaultForm Question where form = gform Nothing

> basil' = basil . const

> basil  :: (a -> M b) -> Web a b
> basil  f = proc a -> do
>    st <- St -< ()
>    (liftAM $ \(refState, a) -> do
>             st <- takeMVar refState
>             let (x, st') = contBasil (f a) st
>             putMVar refState st'
>             return x
>           ) -< (st, a)

> addQuiz :: Web () ()
> addQuiz = proc () -> do
>   q   <- input -< ()
>   ref <- (basil (\x -> new ixQuiz x PNil)) -< q
>   _   <- addQuestions -< ref
>   display (const $ X.toHtml "added quiz with questions.") -< ()

> addQuestions :: Web (Ref QuizModel Quiz) ([Ref QuizModel Question])
> addQuestions = proc q -> do
>   qs  <- inputMany -< ()
>   basil (\(qs,qz) -> mapM (newQuestion qz) qs) -< (qs, q) 

> newQuestion qz question = new ixQuestion question (PCons (qz, DR, ixQuestions) PNil)

> listQuizzes :: Web () ()
> listQuizzes = proc () -> do
>   qs <- basil' (findAll ixQuiz) -< ()
>   display (X.concatHtml . map quizWithLink) -< qs

> viewQuiz :: Web (Ref QuizModel Quiz) ()
> viewQuiz = proc ref -> do
>   quiz <- basil find -< ref
>   Just qsR  <- basil (findRels DL ixQuestions) -< ref
>   qs   <- basil (mapM find) -< S.toList qsR
>   case quiz of
>     Just quiz -> display quizWithQuestions -< (ref, quiz, catMaybes qs)
>     Nothing   -> display (const "Not Found") -< ()

> takeQuiz :: Web (Ref QuizModel Quiz) ()
> takeQuiz = proc ref -> do
>   quiz <- basil find -< ref
>   case quiz of
>     Just quiz ->  do Just qsR  <- basil (findRels DL ixQuestions) -< ref
>                      qs   <- basil (mapM find) -< S.toList qsR
>                      display ghtml -< quiz
>                      f    <- form'' responseForm -< ()
>                      as <- many getAnswer -< (catMaybes qs)
>                      display ghtml -< f {answers = as}
>                      
>     Nothing   -> display (const "Not Found") -< ()

> responseForm = projectedForm responseProj newResponse
>  where newResponse  = Response "" (Email "") (Date "now") []

> getAnswer :: Web Question Answer
> getAnswer = form''' answerForm
>  where answerForm q   = F.plug ((title q X.+++ X.br) X.+++) 
>                       $ F.enumRadio [(QA, choiceA q),(QB, choiceB q),(QC, choiceC q)]
>                                     Nothing

> responseProj :: Response :-> ResponseView
> responseProj = Lens $ ResponseView <$> _name `for` lName <*> _email `for` lEmail


> quizWithQuestions :: (Ref QuizModel Quiz, Quiz, [Question]) -> X.Html
> quizWithQuestions (Ref _ (Fresh i), q, qs) =    ghtml q 
>                           X.+++  X.br
>                           X.+++ (X.concatHtml $ intersperse X.br $ (map ghtml) qs)
>                           X.+++  static (Take i) "take quiz"

> quizWithLink :: (Ref QuizModel Quiz, Quiz) -> X.Html
> quizWithLink (Ref _ (Fresh i), q) =    ghtml q 
>                           X.+++  X.br
>                           X.+++  static (View i) "view"
>                           X.+++  X.br
>                           X.+++  static (Take i) "take quiz"



> handle :: QuizRoute -> Continuation (MVar St) IO ()
> handle Add      = Cont () addQuiz
> handle List     = Cont () listQuizzes
> handle (View i) = Cont (Ref ixQuiz (Fresh i)) viewQuiz
> handle (Take x) = Cont (Ref ixQuiz (Fresh x)) takeQuiz

> main = do
>   ref <- newMVar emptyBasilState :: IO (MVar St)
>   runServer 8080 handle (Env ref M.empty)

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
