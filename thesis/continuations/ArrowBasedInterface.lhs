\begin{spec}
input      :: DefaultForm f => Web () f
display    :: (i -> X.Html) -> Web i ()
link       :: String -> Web i ()

run        :: Env -> String -> RequestBody -> (X.Html, Env) 
runServer  :: Int -> Env ->  IO ()

type Env = M.Map String (Continuation ())

data Continuation o = forall i . Cont i (Web i o)
instance Functor Continuation

data Web i o
instance Arrow        Web
instance Functor      (Web i)
instance ArrowChoice  Web
instance Category     Web

class DefaultForm i  where form :: Form i
instance DefaultForm String
instance DefaultForm Int
instance (DefaultForm a, DefaultForm b) => DefaultForm (a,b)
\end{spec}
