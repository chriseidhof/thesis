\begin{spec}
data Expr entity a where
  Attribute  :: String -> (entity -> att)                       -> Expr entity att
  Constant   :: Show c => c                                     -> Expr entity c
  Equal      :: Eq att   => Expr entity att -> Expr entity att  -> Expr entity Bool
  LT         :: Ord att  => Expr entity att -> Expr entity att  -> Expr entity Bool
  And        :: Expr entity Bool -> Expr entity Bool            -> Expr entity Bool
  Not        :: Expr entity Bool                                -> Expr entity Bool

eval   :: Expr entity a  -> (entity -> a)
toSql  :: Expr entity a  -> String

infix 4 .==.
(.==.) :: Eq att => Expr entity att -> Expr entity att -> Expr entity Bool

infix 4 .<.
(.<.) :: Ord att => Expr entity att -> Expr entity att -> Expr entity Bool

infix 3 .&&.
(.&&.) :: Expr entity Bool -> Expr entity Bool -> Expr entity Bool
\end{spec}
