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
> import Control.Arrow hiding ((+++))
> import Text.XHtml.Strict ((+++), (<<))
> import qualified Text.XHtml.Strict as X
> import qualified Data.Map as M
> import Data.List (intersperse)
> import Data.Maybe (catMaybes)
> import QuizUrl
> import QuizModel
> import qualified Text.XHtml.Strict.Formlets as F

%endif

The next step is to define a |handle| function that takes a |QuizRoute| and
returns a handler. Our handlers are represented as continuations.
The continuations are a combination of an input value of type |i| and a value of
type |Web i ()|.

> handle :: QuizRoute -> Continuation (MVar St) IO ()
> handle Add       = Cont ()                      addQuiz
> handle List      = Cont ()                      listQuizzes
> handle (View i)  = Cont (Ref ixQuiz (Fresh i))  viewQuiz
> handle (Take x)  = Cont (Ref ixQuiz (Fresh x))  takeQuiz

From here one, we will make heavy use of Arrow notation
\cite{paterson2001new}. Using arrow notation, we can conveniently construct web
programs on top of our continuations library from chapter
\ref{chap:continuations}.

\subsection{Adding a Quiz}

To add a quiz, we first show form for entering values of type |Quiz|, we then
add it to the database and proceed by adding questions to the quiz. Finally, we
display a message that it was successfully added.

> addQuiz :: Web () ()
> addQuiz = proc () -> do
>   q    <-  input -< ()
>   ref  <-  (basil (\x -> new ixQuiz x PNil)) -< q
>   _    <-  addQuestions -< ref
>   display (const $ X.toHtml "added quiz with questions.") -< ()

To add a quiz, we let the user input a form using the |input| function. The
|input| function is overloaded using type-classes. Because we use the result of
the |input| value later in the program as a |Quiz| value, the type inferencer
helps us to construct a form that is rendered as a |Quiz| form, displayed in figure \ref{fig:addQuiz}.

\begin{figure}[hb]
\includegraphics[width=15cm]{quiz/screenshots/add-quiz}
\caption{Adding a |Quiz| entity}
\label{fig:addQuiz}
\end{figure}

After the input |q| is entered, we add the quiz to the database, yielding a
reference |ref| to the Quiz value. The database code is wrapped in the |basil|
function. Then |new| function takes an index into the ER model, the entity value
and a list with all the necessary references. For quizzes, this is an empty
list, but we will see a more complicated example later on.

After the quiz is added to the database, we proceed to add questions to the
quiz. We do this with the |addQuestions| function, which uses the reference to
the |Quiz| value.  Finally, we display a message
that the quiz was successfully added.

The |addQuestions| uses the |inputMany| combinator, which is similar to |input|,
but instead shows the form many times, until the user presses |Done|.
An example is rendered in figure \ref{fig:addQuestion}.
After the user is done adding questions, it maps the |newQuestion| function to
each entered question, which stores the question in the database.

> addQuestions :: Web (Ref QuizModel Quiz) ([Ref QuizModel Question])
> addQuestions = proc q -> do
>   qs  <- inputMany -< ()
>   basil (\(qs,qz) -> mapM (newQuestion qz) qs) -< (qs, q) 

\begin{figure}[hb]
\includegraphics[width=15cm]{quiz/screenshots/adding-choices}
\caption{Adding a |Question| entity}
\label{fig:addQuestion}
\end{figure}

The ER modeling library uses type-level functions that all relationships are
correctly used. For example, in our data model, we have specified that each
question belongs to exactly one |Quiz|. Therefore, we also need to supply a
reference to a |Quiz| entity when adding a new |Question| entity to the
database. If we forget to do this, the compiler will give a type-error.

> newQuestion :: Ref QuizModel Quiz -> Question -> M (Ref QuizModel Question)
> newQuestion refQuiz question = new  ixQuestion 
>                                     question 
>                                     (PCons (refQuiz, DR, ixQuestions) PNil)

\subsection{Listing Quizzes}

To list all quizzes, we first fetch all quizzes from the database. If there are
no entries yet, we show a warning, and otherwise we map the |quizWithLink|
function on every quiz. The result of viewing quizzes is show in figure
\ref{fig:listQuiz}

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
> quizWithLink (Ref _ (Fresh i), q) = X.concatHtml
>   [         ghtml q 
>   ,  X.br,  static (View i) "view"
>   ,  X.br,  static (Take i) "take quiz"
>   ]

\begin{figure}[hb]
\includegraphics[width=15cm]{quiz/screenshots/adding-choices}
\caption{Listing |Quiz| entities}
\label{fig:listQuiz}
\end{figure}

\subsection{Viewing Quizzes}

Viewing a quiz looks up the quiz in the database using the |find| function.
It also finds the corresponding questions and displays them. The |findRels|
finds all relations and results in a list of |Ref| values, and the corresponding
entities can be found using the |find| function. Finally, the |display
quizWithQuestions| shows a quiz with its question.

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
>                           +++  X.br
>                           +++ (X.concatHtml $ intersperse X.br $ (map ghtml) qs)
>                           +++  static (Take i) "take quiz"

\subsection{Taking a quiz}

To take a quiz, we first lookup the quiz. If it is found, we find the
corresponding questions, and display the quiz title and description. This first
step is shown in figure \ref{fig:takeQuiz1}.
We use the
|many| combinator, which is of type |Web a b -> Web [a] [b]|. The |getAnswer|
function is of type |Web Question Answer|, and provides an |Answer| for a
question. Using the |many| combinator, we can lift it to work on a list of
questions, yielding a list of answers.
The form for getting a single answer is rendered as in figure \ref{fig:takeQuiz2}.

> takeQuiz :: Web (Ref QuizModel Quiz) ()
> takeQuiz = proc ref -> do
>   quiz <- basil find -< ref
>   case quiz of
>     Just quiz ->  do  
>       Just qsR  <- basil (findRels DL ixQuestions) -< ref
>       qs        <- basil (mapM find) -< S.toList qsR
>       display ghtml -< quiz
>       response  <- form'' responseForm -< ()
>       as        <- many getAnswer -< (catMaybes qs)
>       display ghtml -< response {answers = as}
>                      
>     Nothing   -> display (const "Not Found") -< ()

Note how we used the |form'' responseForm| method to ask for the user's
response. It is rendered in figure \ref{fig:takeQuiz1}. However, it does not
match the structure of the |Response| entity. Yet its result is a value of type
|Response|. We can use the |form''| combinator when we want to customize the
output of a generic program. In this example, we have written a |responseForm|
function that converts between a |Response| and a |ResponseView| value. The
|ResponseView| datatype is rendered as following:

> data ResponseView  = ResponseView { _name :: String
>                                   , _email :: Email
>                                   }

We also define a function for converting between a |Response| and
|ResponseView|. This function is a \emph{lens}, and allows us to convert a
|Response| to a |ResponseView|, but also allows us to take an edited
|ResponseView|, and update the original |Response| with the new value. In web
programming, this happens very often: the form presented only shows fields for
entering part of the data.

> responseProj :: Response :-> ResponseView
> responseProj = Lens $ ResponseView <$> _name `for` lName <*> _email `for` lEmail

Now we can define the |responseForm|, which uses the |projectedForm| function to
display a |ResponseView| form that results in a |Response| value.

> responseForm = projectedForm responseProj newResponse
>  where newResponse  = Response "" (Email "") (Date "now") []

Finally, the function |getAnswer| shows the question and asks the user for
input. It uses a manual form |answerForm| that is written using the formlets
library\footnote{\url{http://hackage.haskell.org/package/formlets}}.

> getAnswer :: Web Question Answer
> getAnswer = form''' answerForm
>  where answerForm q   = F.plug ((title q +++ X.br) +++) 
>                       $ F.enumRadio [(QA, choiceA q),(QB, choiceB q),(QC, choiceC q)]
>                                     Nothing

\begin{figure}
\includegraphics[width=15cm]{quiz/screenshots/taking-quiz}
\caption{Taking a quiz: the first step}
\label{fig:takeQuiz1}
\end{figure}

\begin{figure}
\includegraphics[width=15cm]{quiz/screenshots/taking-quiz2}
\caption{Taking a quiz: answering a question}
\label{fig:takeQuiz2}
\end{figure}

\subsection{Miscellaneous functions}

We can run the server using the |runServer| function, which takes the port, the
|homePage|, the dispatch function |handle| and an environment.

> main = do
>   ref <- newMVar emptyBasilState :: IO (MVar St)
>   runServer 8080 homePage template handle (Env ref M.empty)

The |homePage| is a static |Html| page that shows a list of actions.

> homePage = X.toHtml "Welcome to the quiz system."

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
>          +++ (X.body $
>            templateHeader +++ x)
>            )
> templateHeader = (X.h1 X.! [X.theclass "page"]) (X.toHtml "Quiz system") +++ menu
>
> menu = X.unordList X.! [X.theclass "menu"] $ [static Add "add quiz", static List "list quizzes"]
>
> linkStyle name = (X.thelink X.! [X.rel "stylesheet", X.href name]) X.noHtml


Finally, we need to give the |Regular| instances for |QuizForm| and
|ResponseView|:

> $(deriveAll ''QuizForm  "PFQuizForm")
> type instance PF QuizForm = PFQuizForm

> $(deriveAll ''ResponseView  "PFResponseView")
> type instance PF ResponseView = PFResponseView
