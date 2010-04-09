\begin{spec}
input    :: DefaultForm a => Web a
display  :: X.HTML h => h -> Web ()
link     :: String -> Web ()

run              :: Env -> String -> RequestBody -> (X.Html, Env) 
runServer        :: Int -> Env -> IO ()
createServerPart :: Env -> IO (ServerPart Response)

type Env         = M.Map String (Web ())
type RequestBody = [(String,String)]

newtype Web a
instance Functor  Web
instance Monad    Web

class DefaultForm i  where form :: Form i
instance DefaultForm  String
instance DefaultForm  Int
instance (DefaultForm a, DefaultForm b) => DefaultForm (a,b)
\end{spec}
