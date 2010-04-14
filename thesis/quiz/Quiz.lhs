%if False

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

%endif

The |handle| function takes a |QuizRoute| and returns a continuation. The
|QuizRoute| is automatically constructed from the url. The continuations are a
combination of an input value |i| and a |Web i ()| value. They have an |MVar St|
available, which is a reference to the database.

> handle :: QuizRoute -> Continuation (MVar St) IO ()
> handle Add       = Cont ()                      addQuiz
> handle List      = Cont ()                      listQuizzes
> handle (View i)  = Cont (Ref ixQuiz (Fresh i))  viewQuiz
> handle (Take x)  = Cont (Ref ixQuiz (Fresh x))  takeQuiz

Because URLs form part of the user interface, we have chosen to use a hybrid
approach using both continuations and static URLs.
A value |View 5| is rendered as the url \texttt{/view/5}, which allows the user to link
to pages.

To add a quiz, we let the user input a form using the |input| function.  Then we
add the quiz to the database, yielding a reference |ref| to the Quiz value,
which we use for adding questions to the quiz.  Finally, we display a message
that the quiz was successfully added. We make heavy use of Arrow notation
\cite{paterson2001new}. This style of constructing web programs uses our
continuations library from chapter \ref{chap:continuations} and the in-memory
interface for ER models described in section \ref{sec:inmeminterface}. The input
function builds a form generically, using the generic programming techniques
described in chapter \ref{chap:views}.

> addQuiz :: Web () ()
> addQuiz = proc () -> do
>   q    <-  input -< ()
>   ref  <-  (basil (\x -> new ixQuiz x PNil)) -< q
>   _    <-  addQuestions -< ref
>   display (const $ X.toHtml "added quiz with questions.") -< ()

The |addQuestions| uses the |inputMany| combinator, which is similar to |input|,
but instead shows the form many times, until the user presses |Done|.
Afterwards, it maps the |newQuestion| function to each entered question.

> addQuestions :: Web (Ref QuizModel Quiz) ([Ref QuizModel Question])
> addQuestions = proc q -> do
>   qs  <- inputMany -< ()
>   basil (\(qs,qz) -> mapM (newQuestion qz) qs) -< (qs, q) 

The |newQuestion| stores the question in the database. Because a question always
belongs to exactly one |Quiz|, we also need to supply a reference to the |Quiz|
model.

> newQuestion :: Ref QuizModel Quiz -> Question -> M (Ref QuizModel Question)
> newQuestion qz question = new ixQuestion question (PCons (qz, DR, ixQuestions) PNil)

To list all quizzes, we first fetch all quizzes from the database. If there are
no entries yet, we show a warning, and otherwise we map the |quizWithLink|
function on every quiz.

> listQuizzes :: Web () ()
> listQuizzes = proc () -> do
>   qs <- basil' (findAll ixQuiz) -< ()
>   case qs of
>     []  -> display X.toHtml -< "No quizzes yet."
>     _   -> display (X.concatHtml . map quizWithLink) -< qs

The |quizWithLink| takes a |Quiz| and its reference and produces the Html for
that quiz. It uses the |ghtml| function to show the Html for a quiz and
concatenates a link to either view or take the quiz.

> quizWithLink :: (Ref QuizModel Quiz, Quiz) -> X.Html
> quizWithLink (Ref _ (Fresh i), q) = 
>          ghtml q 
>   X.+++  X.br
>   X.+++  static (View i) "view"
>   X.+++  X.br
>   X.+++  static (Take i) "take quiz"

Viewing a quiz looks up the quiz in the database. It also finds the
corresponding questions and displays them. Finally, it displays the quiz with
its questions.

> viewQuiz :: Web (Ref QuizModel Quiz) ()
> viewQuiz = proc ref -> do
>   quiz <- basil find -< ref
>   case quiz of
>     Just quiz -> do  Just qsR  <- basil (findRels DL ixQuestions) -< ref
>                      qs        <- basil (mapM find) -< S.toList qsR
>                      display quizWithQuestions -< (ref, quiz, catMaybes qs)
>     Nothing   -> display (const "Not Found") -< ()

The function |quizwithQuestions| is very similar to |quizWithLink|, except that
it also displays all the questions.

> quizWithQuestions :: (Ref QuizModel Quiz, Quiz, [Question]) -> X.Html
> quizWithQuestions (Ref _ (Fresh i), q, qs) =    ghtml q 
>                           X.+++  X.br
>                           X.+++ (X.concatHtml $ intersperse X.br $ (map ghtml) qs)
>                           X.+++  static (Take i) "take quiz"

To take a quiz, we first lookup the quiz. If it is found, we find the
corresponding questions, and display the quiz title and description. We use the
|many| combinator, which is of type |Web a b -> Web [a] [b]|. So instead of
converting one input value |a| into a |b|, it converts all values in |[a]| into
|[b]|. In this case, it performs the |getAnswer| for all the questions.

> takeQuiz :: Web (Ref QuizModel Quiz) ()
> takeQuiz = proc ref -> do
>   quiz <- basil find -< ref
>   case quiz of
>     Just quiz ->  do  
>       Just qsR  <- basil (findRels DL ixQuestions) -< ref
>       qs        <- basil (mapM find) -< S.toList qsR
>       display ghtml -< quiz
>       f         <- form'' responseForm -< ()
>       as        <- many getAnswer -< (catMaybes qs)
>       display ghtml -< f {answers = as}
>                      
>     Nothing   -> display (const "Not Found") -< ()

Recall that a response stored the user's name, email, and the date the response
was given.
Instead of letting the user enter the date, we use a programatically generated
date.
Therefore, we add a new datataype |ResponseView| that only shows the name and
email fields.

> data ResponseView  = ResponseView { _name :: String
>                                   , _email :: Email
>                                   }

We also give a lens for converting between a |Response| and |ResponseView|:

> responseProj :: Response :-> ResponseView
> responseProj = Lens $ ResponseView <$> _name `for` lName <*> _email `for` lEmail

Now we can define the |responseForm|, which uses the |projectedForm| function to
display a |ResponseView| form that results in a |Response| value.

> responseForm = projectedForm responseProj newResponse
>  where newResponse  = Response "" (Email "") (Date "now") []

Finally, the function |getAnswer| shows the question and asks the user for
input. It uses a manual form |answerForm| that is written using the formlets
library.

> getAnswer :: Web Question Answer
> getAnswer = form''' answerForm
>  where answerForm q   = F.plug ((title q X.+++ X.br) X.+++) 
>                       $ F.enumRadio [(QA, choiceA q),(QB, choiceB q),(QC, choiceC q)]
>                                     Nothing

We can run the server using the |runServer| function, which takes the port, the
|homePage|, the dispatch function |handle| and an environment.

> main = do
>   ref <- newMVar emptyBasilState :: IO (MVar St)
>   runServer 8080 homePage template handle (Env ref M.empty)

The |homePage| is a static |Html| page that shows a list of actions.

> homePage = X.unordList [static Add "add quiz", static List "list quizzes"]

\subsection{Helper functions}

Now we will provide the instances for forms. For a |Quiz|, we again use a
modified form, because we want to render the description input field as a
multiline input field, which is done using the |TextArea| combinator provided by
the generic programming library.

> instance DefaultForm Quiz     where form = projectedForm quizProj (Quiz "" "")

> data QuizForm = QuizForm {_subject :: String, _description :: TextArea}
>
> quizProj :: Quiz :-> QuizForm
> quizProj = Lens $ QuizForm  <$>  _subject      `for` lSubject 
>                             <*>  _description  `for` (textAreaToString % lDescription)

For |Question|s, we use the normal generic forms.

> instance DefaultForm Question where form = gform Nothing

We also have used some helper types and functions.

> type M a     = Basil QuizModel QuizRelations a
> type St      = BasilState QuizModel QuizRelations
> type Web i o = WebT (MVar St) IO i o

The |basil| function lifts a database action into a |Web| value.

> basil  :: (a -> M b) -> Web a b
> basil  f = proc a -> do
>    st <- St -< ()
>    (liftAM $ \(refState, a) -> do
>             st <- takeMVar refState
>             let (x, st') = contBasil (f a) st
>             putMVar refState st'
>             return x
>           ) -< (st, a)

The function |basil'| is a utility function that ignore the argument.

> basil' :: M b -> Web a b
> basil' = basil . const

The template that we use:

> template x = X.thehtml (
>          (X.header  $ linkStyle "/public/style.css")
>          X.+++ (X.body x)
>            )

> linkStyle name = (X.thelink X.! [X.rel "stylesheet", X.href name]) X.noHtml


Finally, we need to give the |Regular| instances for |QuizForm| and
|ResponseView|:

> $(deriveAll ''QuizForm  "PFQuizForm")
> type instance PF QuizForm = PFQuizForm

> $(deriveAll ''ResponseView  "PFResponseView")
> type instance PF ResponseView = PFResponseView
