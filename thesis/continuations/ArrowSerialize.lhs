We now give a description to serialize arrows.
Note that we have not yet implemented this, it is future work.
We have identified two approaches that can be used to serialize |Web| values:
 
 \begin{itemize}
 \item
In order to serialize an |Arrow|, we can keep track of which steps which have
already been taken. Consider the |handleRequest| function defined above,
which changes the |Web| structure. 
If we store how it changes the |Web| structure (for example, in the
|Continuation| datatype), we can always reconstruct the same |Web| value
from our original |Web| value.
We can store the changes by introducing a |Trace| datatype, and every rewrite
step emits a |Trace| value. That way, we can reinterpret the |Web| and a |Trace|
value, and reconstruct the original |Web| value. Using a |Dynamic|
value\footnote{\url{http://haskell.org/ghc/docs/latest/html/libraries/base-4.2.0.0/Data-Dynamic.html}},
we can cast the input value to the right type in a safe way. We think that the
generalized |Zipper| data-structure \cite{hinze-chapter} might be of help here.

\item
A |Web| value can be recursive, or in other words: it is a \emph{graph} that
describes the computation. However, the code that is used to describe this graph
is finite.
Using the data-reify
package\footnote{\url{http://hackage.haskell.org/package/data-reify}}
\citet{gill2009type}.
we can inspect sharing among values, and convert the graph value into a finite
representation, where each |Web| value is uniquely identified by a number.
We can then store a continuation as the number and the input value.

\end{itemize}

%if False

> {-# LANGUAGE GADTs, ExistentialQuantification, TypeSynonymInstances,
> FlexibleContexts, PackageImports, Arrows #-}
> module ArrowSerialize where

> import "mtl" Control.Monad.Identity (Identity (..))
> import "transformers" Control.Monad.Trans
> import Control.Applicative
> import Control.Applicative.Error (Failing (..))
> import Control.Arrow
> import Control.Category
> import Control.Concurrent.MVar
> import Control.Monad.Reader hiding (liftIO)
> import Data.List (intercalate)
> import Data.Maybe (isJust, fromJust)
> import Data.Record.Label
> import Network.Protocol.Http
> import Network.Protocol.Http.Data
> import Network.Protocol.Uri
> import Network.Protocol.Uri.Data
> import Network.Salvia.Handlers
> import Network.Salvia.Impl.Config
> import Network.Salvia.Impl.Server
> import Network.Salvia.Interface
> import Prelude as Prelude hiding ((.))
> import qualified Data.ByteString.Lazy.Char8 as B
> import qualified Data.Map as M
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as Formlets
> import ArrowBased hiding (handleRequest, runPage, returnCont, noResult)


data Web i o where
  Single  :: Page i o -> Web i o
  Req     :: Web () RequestBody
  Seq     :: Web a b -> Web b c -> Web a c
  First   :: Web a b -> Web (a, c) (b, c)
  Choice  :: Web a b -> Web (Either a c) (Either b c)

> data Trace = TrSeq | TrLeft | TrRight
>  deriving Show

> data Result2 o where
>   Done2   :: o -> Result2 o
>   Step2   :: X.Html -> Continuation2 o -> Result2 o

The |Continuation| is not just a |Web i o| value, but also stores the input |i|.
We can wrap this in an existential type.

> data Continuation2 o = forall i . Cont2 i [Trace] (Web i o)

> addTr _  (Done2 x)   = Done2 x
> addTr ls (Step2 h c) = Step2 h (addTr' c ls)

> addTr' (Cont2 i tr w) ls = Cont2 i (ls ++ tr) w

> instance Functor Continuation2 where
>   fmap f (Cont2 i s w) = Cont2 i s (fmap f w)
> 
> instance Show (Continuation2 o) where 
>   show (Cont2 i tr w) = show (length tr) ++ (take 120 $ show w)

> instance Show o => Show (Result2 o) where
>   show (Done2 x) = show x
>   show (Step2 h c) = "Step2 (" ++ show h ++ ", " ++ show c ++ ")"


> instance Functor Result2 where
>   fmap f (Done2 x)   = Done2 (f x)
>   fmap f (Step2 h c) = Step2 h (fmap f c)

To get the result of a single page, we provide the |runPage| function, which is
again very similar to the |runPage| function in the previous section.

> runPage :: Page i o -> i -> NextPage -> Result2 o
> runPage (Fun f)           i np = Done2  (f i)
> runPage (Form msg parse)  _ np = Step2  (makeForm msg) 
>                                         (returnCont $ runForm msg parse)
> runPage (Display msg)     i np = Step2  (continue (msg i) np "Continue") 
>                                         noResult
> runPage (Link  s)         i np = Step2  (continue X.noHtml np s)
>                                         noResult

The helper functions |noResult| and |returnCont| build simple continuations:

> noResult :: Continuation2 ()
> noResult = returnCont (arr (const ()))

> returnCont :: Web () o -> Continuation2 o
> returnCont = Cont2 () []

> handleRequest :: Web i o -> i -> NextPage -> RequestBody -> Result2 o
> handleRequest (Req)         inp np body = Done2 body
> handleRequest (Single page) inp np body = runPage page inp np
> handleRequest (Seq    l r)  inp np body = case handleRequest l inp np body of
>   Done2 res                  -> addTr [TrSeq] $ handleRequest r res np body
>   Step2 page (Cont2 i tr c)  -> Step2 page (Cont2 i tr (c `Seq` r))
> handleRequest (First l) (i1,i2) np body = case handleRequest l i1 np body of
>     Done2 res              -> Done2 (res, i2)
>     Step2 page (Cont2 i tr c)  -> Step2 page (Cont2 (i,i2) tr (First c))
> handleRequest (Choice a) (Left   inp) np body = Left <$> handleRequest a inp np body
> handleRequest (Choice a) (Right  inp) np body = Done2 (Right inp)

> handleRequest' :: Continuation2 o -> NextPage -> RequestBody -> Result2 o
> handleRequest' (Cont2 i tr c) np = addTr tr . handleRequest c i np 

%endif
