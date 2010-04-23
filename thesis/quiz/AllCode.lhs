\subsection{The model}

> newtype Email = Email { unEmail :: String }
> newtype Date  = Date  { unDate  :: String }
>
> instance Html Email where html = html . unEmail
> instance Html Date where html = html . unDate
> instance Html [Answer] where html = html . concatMap show
>
> instance Formlet Email where formlet x = Email <$> formlet (unEmail <$> x)
>
> data Quiz      = Quiz      {  subject :: String, description :: String}
> data Question  = Question  {  title :: String, choiceA, choiceB, choiceC :: String}
> data Response  = Response  {  name     :: String
>                            ,  email    :: Email
>                            ,  date     :: Date
>                            ,  answers  :: [Answer] 
>                            }
>
> data Answer    = QA | QB | QC deriving (Show, Eq, Enum)
>
> type QuizModel = Quiz B.:*: Question B.:*: Response B.:*: Nil
>
> $(mkIndexes ''QuizModel  [''Quiz, ''Question, ''Response])
>
> type Questions  = Rel  QuizModel  One   Quiz  Many  Question
> type Responses  = Rel  QuizModel  One   Quiz  Many  Response
>
> questions   :: Questions
> questions  = Rel  One   ixQuiz  "quiz"  Many  ixQuestion  "questions"
>
> responses   :: Responses
> responses  = Rel  One   ixQuiz  "quiz"  Many  ixResponse "responses"
>
> type QuizRelations = Questions B.:*: Responses B.:*: Nil
>
> instance ERModel QuizModel QuizRelations where
>   relations  =  TCons4 questions
>              $  TCons4 responses
>              $  TNil4 
>   witnesses  =  WCons ixQuiz $  WCons ixQuestion $  WCons ixResponse $  WNil
>
> $(mkIndexes ''QuizRelations [''Questions, ''Responses])
>
> $(deriveAll ''Quiz  "PFQuiz")
> $(deriveAll ''Question  "PFQuestion")
> $(deriveAll ''Response  "PFResponse")
>
> type instance PF Quiz = PFQuiz
> type instance PF Question = PFQuestion
> type instance PF Response = PFResponse
>
> $(mkLabels [''Quiz, ''Question, ''Response])
> $(mkEqualities [''Quiz, ''Question, ''Response])

\subsection{The URL routing}

> data QuizRoute  =  List
>                 |  Add
>                 |  View  Int
>                 |  Take  Int
>   deriving Show
>
> instance ToURL QuizRoute where
>  toURL    = gtoURL  . from
>  fromURL  = fmap to . gfromURL
>
> $(deriveAll ''QuizRoute "PFQuizRoute")
> type instance PF QuizRoute = PFQuizRoute
>
> static :: QuizRoute -> String -> X.Html
> static u s = X.anchor X.! [X.href $ "/" ++ renderURL (toURL u)] X.<< X.toHtml s

\subsection{The Quiz controller}

> handle :: QuizRoute -> Continuation (MVar St) IO ()
> handle Add       = Cont ()              addQuiz
> handle List      = Cont ()              listQuizzes
> handle (View i)  = Cont (Ref ixQuiz i)  viewQuiz
> handle (Take i)  = Cont (Ref ixQuiz i)  takeQuiz
>
> addQuiz :: Web () ()
> addQuiz = proc () -> do
>   q    <-  input                                           -< ()
>   ref  <-  (basil (\x -> new ixQuiz x PNil))               -< q
>   _    <-  addQuestions                                    -< ref
>   display (const $ X.toHtml "added quiz with questions.")  -< ()
>
> addQuestions :: Web (Ref QuizModel Quiz) ([Ref QuizModel Question])
> addQuestions = proc q -> do
>   qs  <- inputMany                              -< ()
>   basil (\(qs,qz) -> mapM (newQuestion qz) qs)  -< (qs, q) 
>
> newQuestion :: Ref QuizModel Quiz -> Question -> M (Ref QuizModel Question)
> newQuestion refQuiz question = new  ixQuestion 
>                                     question 
>                                     (PCons (refQuiz, DR, ixQuestions) PNil)
>
> listQuizzes :: Web () ()
> listQuizzes = proc () -> do
>   qs <- basil' (findAll ixQuiz) -< ()
>   case qs of
>     []  -> display X.toHtml                           -< "No quizzes yet."
>     _   -> display (X.concatHtml . map quizWithLink)  -< qs
>
> quizWithLink :: (Ref QuizModel Quiz, Quiz) -> X.Html
> quizWithLink (Ref _ i, q) = X.concatHtml
>   [         ghtml q 
>   ,  X.br,  static (View i) "view"
>   ,  X.br,  static (Take i) "take quiz"
>   ]
>
> viewQuiz :: Web (Ref QuizModel Quiz) ()
> viewQuiz = proc ref -> do
>   quiz <- basil find -< ref
>   case quiz of
>     Just quiz -> do  
>       Just qsR  <- basil (findRels DL ixQuestions)  -< ref
>       qs        <- basil (mapM find)                -< S.toList qsR
>       display quizWithQuestions                     -< (ref, quiz, catMaybes qs)
>     Nothing   -> display (const "Not Found") -< ()
>
> quizWithQuestions :: (Ref QuizModel Quiz, Quiz, [Question]) -> X.Html
> quizWithQuestions (Ref _ i, q, qs) =    ghtml q 
>                           +++  X.br
>                           +++ (X.concatHtml $ intersperse X.br $ (map ghtml) qs)
>                           +++  static (Take i) "take quiz"
>
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
>
> data ResponseView  = ResponseView  {  _name   :: String
>                                    ,  _email  :: Email
>                                    } 
>
> responseProj :: Response :-> ResponseView
> responseProj = Lens $ ResponseView <$> _name `for` lName <*> _email `for` lEmail
>
> responseForm :: Form Response
> responseForm = projectedForm responseProj emptyResponse
>  where emptyResponse :: Response
>        emptyResponse  = Response "" (Email "") (Date "now") []
>
> getAnswer :: Web Question Answer
> getAnswer = form''' answerForm
>
> answerForm :: Question -> Form Answer
> answerForm q   = F.plug ((title q +++ X.br) +++) 
>                $ F.enumRadio  [(QA, choiceA q),(QB, choiceB q),(QC, choiceC q)]
>                               Nothing
>
> main = do
>   ref <- newMVar emptyBasilState :: IO (MVar St)
>   runServer 8080 homepage template handle (Env ref M.empty)
>
> homepage = X.toHtml "Welcome to the quiz system."
>
> data QuizForm = QuizForm {_subject :: String, _description :: TextArea}
>
> quizProj :: Quiz :-> QuizForm
> quizProj = Lens $ QuizForm  <$>  _subject      `for` lSubject 
>                             <*>  _description  `for` (textAreaToString % lDescription)
>
> instance DefaultForm Quiz     where form = projectedForm quizProj (Quiz "" "")
> instance DefaultForm Question where form = gform Nothing
>
> type M a     = Basil QuizModel QuizRelations a
> type St      = BasilState QuizModel QuizRelations
> type Web i o = WebT (MVar St) IO i o
>
> basil  :: (a -> M b) -> Web a b
> basil  f = proc a -> do
>    st <- St -< ()
>    (liftAM $ \(refState, a) -> do
>             st <- takeMVar refState
>             let (x, st') = contBasil (f a) st
>             putMVar refState st'
>             return x
>     ) -< (st, a)
>
> basil' :: M b -> Web a b
> basil' = basil . const
>
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
>
> $(deriveAll ''QuizForm  "PFQuizForm")
> type instance PF QuizForm = PFQuizForm
>
> $(deriveAll ''ResponseView  "PFResponseView")
> type instance PF ResponseView = PFResponseView
