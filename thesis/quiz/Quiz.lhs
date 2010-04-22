%if False

> {-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeSynonymInstances,
>     TypeFamilies, FlexibleContexts, RankNTypes, Arrows, TemplateHaskell,
>     EmptyDataDecls
>   #-}

> module Quiz where


> import Basil hiding ((:*:))
> import Basil.InMemory
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
The continuations combine an input value of type |i| and a value of
type |Web i ()|. When we add or list a quiz, there is no input. However, when we
|View| or |Take| a quiz, we provide a reference to the |Quiz|. This reference is
constructed from the information in the |QuizRoute| datatype.

> handle :: QuizRoute -> Continuation (MVar St) IO ()
> handle Add       = Cont ()              addQuiz
> handle List      = Cont ()              listQuizzes
> handle (View i)  = Cont (Ref ixQuiz i)  viewQuiz
> handle (Take i)  = Cont (Ref ixQuiz i)  takeQuiz

From here one, we make heavy use of arrow notation
\cite{paterson2001new}. Using the arrow notation, we can conveniently construct web
programs on top of our continuations library from chapter \ref{chap:continuations}.

\subsection{Adding a new Quiz}

To add a quiz, we first show the form for entering values of type |Quiz|, we then
add it to the database and proceed by adding questions to the quiz. Finally, we
display a message that it was successfully added. 
The type |Web () ()| means that the input of this function is a value of type
|()|, and the output is also a value of type |()|. In general, a function of
type |Web a b| has a value of type |a| as it input and produces a value of type
|b|.

> addQuiz :: Web () ()
> addQuiz = proc () -> do
>   q    <-  input                                           -< ()
>   ref  <-  (basil (\x -> new ixQuiz x PNil))               -< q
>   _    <-  addQuestions                                    -< ref
>   display (const $ X.toHtml "added quiz with questions.")  -< ()

To add a quiz, we let the user input a form using the overloaded |input| function:
because we use the result of
the |input| value later in the program as a |Quiz| value, the type inferencer
constructs a form that is rendered as a |Quiz| form (see figure
\ref{fig:addQuiz}).

\begin{figure}[hb]
\includegraphics[width=15cm]{quiz/screenshots/add-quiz}
\caption{Adding a |Quiz| entity}
\label{fig:addQuiz}
\end{figure}

After the input |q| is entered and bound to variable |q|, we add the quiz to the database, yielding a
reference |ref| to the Quiz value.
The database code is wrapped in the |basil|
function. The |new| function takes as its first paramater an index into the ER
model, which refers to the type of the entity that is created.
The second parameter is the the entity, and the third parameter is a list with all the necessary references.
For quizzes, the third argument is the empty list |PNil|, we show a more complicated example later on.

After the quiz is added to the database, we proceed to add questions to the
quiz. We do this with the |addQuestions| function, using the reference to the |Quiz| value.
Finally, we display a message that the quiz was successfully added.

The function |addQuestions| uses the |inputMany| combinator, which differs from |input|:
it shows a form many times, until the user presses \emph{Done} (see figure
\ref{fig:addQuestion}).
The function |newQuestion| stores a |Question| in the database, and it is
applied on each entered question.

> addQuestions :: Web (Ref QuizModel Quiz) ([Ref QuizModel Question])
> addQuestions = proc q -> do
>   qs  <- inputMany                              -< ()
>   basil (\(qs,qz) -> mapM (newQuestion qz) qs)  -< (qs, q) 

\begin{figure}[hb]
\includegraphics[width=15cm]{quiz/screenshots/adding-choices}
\caption{Adding a |Question| entity}
\label{fig:addQuestion}
\end{figure}

The ER modeling library uses type-level functions which enforces all relationships are
correctly used and initialized.
For example, in our data model, we have specified that each
question belongs to exactly one |Quiz|.
Therefore, we need to supply a reference to a |Quiz| entity when adding a new |Question| entity to the
database. If we forget to do this, the compiler gives a type-error.

> newQuestion :: Ref QuizModel Quiz -> Question -> M (Ref QuizModel Question)
> newQuestion refQuiz question = new  ixQuestion 
>                                     question 
>                                     (PCons (refQuiz, DR, ixQuestions) PNil)

\subsection{Listing Quizzes}

To list all quizzes (see figure \ref{fig:listQuiz}), we fetch all quizzes from the database. If there are
no quizzes yet, we show a warning, otherwise we apply the |quizWithLink| function on every quiz.

> listQuizzes :: Web () ()
> listQuizzes = proc () -> do
>   qs <- basil' (findAll ixQuiz) -< ()
>   case qs of
>     []  -> display X.toHtml                           -< "No quizzes yet."
>     _   -> display (X.concatHtml . map quizWithLink)  -< qs

The |quizWithLink| takes a |Quiz| and its reference and produces HTML for
that quiz. Recall that we defined |Quiz| as a record type: the |ghtml| displays
a line with each record field and its value.
Finally, we add links to view or take the quiz.

> quizWithLink :: (Ref QuizModel Quiz, Quiz) -> X.Html
> quizWithLink (Ref _ i, q) = X.concatHtml
>   [         ghtml q 
>   ,  X.br,  static (View i) "view"
>   ,  X.br,  static (Take i) "take quiz"
>   ]

\begin{figure}[hb]
\includegraphics[width=15cm]{quiz/screenshots/list-quizzes}
\caption{Listing |Quiz| entities}
\label{fig:listQuiz}
\end{figure}

\subsection{Viewing Quizzes}

To view a quiz, the function |viewQuiz| starts by looking up the 
quiz in the database, using the |find| function, followed by locating the
corresponding questions and displaying them. The |findRels|
finds all relations and results in a list of |Ref| values. The corresponding
entities can be found using the |find| function. Finally, the |display
quizWithQuestions| shows a quiz with its question.

> viewQuiz :: Web (Ref QuizModel Quiz) ()
> viewQuiz = proc ref -> do
>   quiz <- basil find -< ref
>   case quiz of
>     Just quiz -> do  
>       Just qsR  <- basil (findRels DL ixQuestions)  -< ref
>       qs        <- basil (mapM find)                -< S.toList qsR
>       display quizWithQuestions                     -< (ref, quiz, catMaybes qs)
>     Nothing   -> display (const "Not Found") -< ()

The function |quizwithQuestions| is very similar to |quizWithLink|, except that
it also displays all the questions.

> quizWithQuestions :: (Ref QuizModel Quiz, Quiz, [Question]) -> X.Html
> quizWithQuestions (Ref _ i, q, qs) =    ghtml q 
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
>       Just qsR  <- basil (findRels DL ixQuestions)  -< ref
>       qs        <- basil (mapM find)                -< S.toList qsR
>       ()        <- display ghtml                    -< quiz
>       response  <- form'' responseForm              -< ()
>       as        <- many getAnswer                   -< (catMaybes qs)
>       display ghtml                                 -< response {answers = as}
>                      
>     Nothing   -> display (const "Not Found") -< ()

Note how we used the |form'' responseForm| method to ask for the user's
response. In the |Response| type, there is a |date| field that stores when the
response was added. However, we do not present the date field to the user, it is
filled in programmatically. To do this, we first introduce a new datatype
|ResponseView|:

> data ResponseView  = ResponseView  {  _name   :: String
>                                    ,  _email  :: Email
>                                    } 

Second, we define a function for converting between a |Response| and
|ResponseView|. This function is called a \emph{lens} \cite{relationallenses}, and allows us to convert a
|Response| to a |ResponseView|, but also allows us to take an edited
|ResponseView|, and update the original |Response| with the new value. We have
constructed the lens using the fclabels package \footnote{\url{http://hackage.haskell.org/package/fclabels}}.

> responseProj :: Response :-> ResponseView
> responseProj = Lens $ ResponseView <$> _name `for` lName <*> _email `for` lEmail

Now we can define the function |responseForm| that is used in |takeQuiz|. We
define it using the function |projectedForm|, which takes two parameter. The
first parameter is the lens between |Response| and |ResponseForm|. The second
parameter is an empty value of type |Response|. After the form is filled in,
the empty value is updated with the new values from |ResponseView|:

> responseForm :: Form Response
> responseForm = projectedForm responseProj emptyResponse
>  where emptyResponse :: Response
>        emptyResponse  = Response "" (Email "") (Date "now") []

We have now changed a generic form by introducing a new datatype and a
function to convert between the original datatype (the \emph{model datatype}) and
the new datatype (the \emph{view datatype}). This is a powerful technique for
customizing the code generated by generic programming.

The function |getAnswer| displays the question and asks the user for
an answer. It uses a manual form |answerForm| that is written using the formlets
library\footnote{\url{http://hackage.haskell.org/package/formlets}}.

> getAnswer :: Web Question Answer
> getAnswer = form''' answerForm

> answerForm :: Question -> Form Answer
> answerForm q   = F.plug ((title q +++ X.br) +++) 
>                $ F.enumRadio  [(QA, choiceA q),(QB, choiceB q),(QC, choiceC q)]
>                               Nothing

At this point, we have defined the quiz model, the URL routing and the URL
handlers.

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

We can run the server using the |runServer| function, which takes the port, a
homepage, the dispatch function |handle| and an environment:

> main = do
>   ref <- newMVar emptyBasilState :: IO (MVar St)
>   runServer 8080 homepage template handle (Env ref M.empty)

The |homePage| is a static HTML page that shows a welcome message.

> homepage = X.toHtml "Welcome to the quiz system."

Recall that a |Quiz| and |Question| were rendered using the |input| and
|inputMany| combinator. To do this, we need to make |Quiz| and |Question| an
instance of the |DefaultForm| typeclass.

We start by providing the |DefaultForm| instance for |Quiz|.
Recall that the |Quiz| datatype constists of a |subject| and a |description|
field, which are both of type |String|.
The default form component for a |String| is a single line input field,
However, we want to render the description as a multiline input field (see
figure \ref{fig:addQuiz}).
We achieve this by again building a custom view datatype
|QuizForm|.
The |_description| field of the |QuizForm| type has type |TextArea|, which is
provided by the generic programming library, and is rendered as a multiline
input field.

> data QuizForm = QuizForm {_subject :: String, _description :: TextArea}

Again, we write a lens between |Quiz| and |QuizForm|. Because the types of
|lDescription| and |_description| differ, we need a way to convert between
values of the different types. This is done by the |textAreaToString|, which
provides an isomorphism between |TextArea| and |String|. This function is also
provided by the generic programming library. The |%| operator takes an
isomorphism as its left operand, a lens as its right operand, and produces a new
lens with a changed output type:

> quizProj :: Quiz :-> QuizForm
> quizProj = Lens $ QuizForm  <$>  _subject      `for` lSubject 
>                             <*>  _description  `for` (textAreaToString % lDescription)

The instance for |DefaultForm| now again uses the |projectedForm| function:

> instance DefaultForm Quiz     where form = projectedForm quizProj (Quiz "" "")

For |Question|s, we use the normal generic forms provided by the generic
programming library:

> instance DefaultForm Question where form = gform Nothing

We also have used some convenient helper types. The |M a| is the monad in which
our database computations run, and the |St| type is the type of the global
state. Finally, the |Web| type is the type of our handler functions. It
instantiates the |WebT| type with an |MVar| that holds the in-memory database.

> type M a     = Basil QuizModel QuizRelations a
> type St      = BasilState QuizModel QuizRelations
> type Web i o = WebT (MVar St) IO i o

The |basil| function lifts a database action into a |Web| value. It uses the
in-memory database features provided by the data modeling library. The in-memory
database is stored in an |MVar|. Therefore, a database action results in a
change of the |MVar|'s value.

> basil  :: (a -> M b) -> Web a b
> basil  f = proc a -> do
>    st <- St -< ()
>    (liftAM $ \(refState, a) -> do
>             st <- takeMVar refState
>             let (x, st') = contBasil (f a) st
>             putMVar refState st'
>             return x
>     ) -< (st, a)

The function |basil'| is a utility function that ignores the argument.

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
