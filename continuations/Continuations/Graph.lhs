> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ExistentialQuantification #-}
> 
> module Continuations.Graph (explicitSharing, MuCont(..)) where
> 
> import Data.Reify
> import Control.Applicative
> import Continuations.Types
 
First, we need a representation of our datatypes with the recursive points abstracted away.

> data MuCont u where
>       FAction :: (a -> Action b) -> MuCont u
>       FEdge :: u -> u -> MuCont u
>       FChoice :: (a -> Bool) -> u -> u -> MuCont u
>       --FThread  :: u -> MuCont u
> 
> instance (Show u) => Show (MuCont u) where
>   show (FEdge l r) = "FEdge (" ++ show l ++ ") (" ++ show r ++ ")"
>   show (FAction x) = "FAction"
>   show (FChoice cond l r) = "FChoice cond (" ++ show l ++ ") (" ++ show r ++ ")"
>   --show (FThread t ) = "FThread (" ++ show t ++ ")"
> 
> 
> instance Functor MuCont where
>   fmap f (FAction x) = FAction x
>   fmap f (FEdge a b) = FEdge (f a) (f b)
>   fmap f (FChoice cond l r) = FChoice cond (f l) (f r)
>   --fmap f (FThread t) = FThread (f t)
> 

We need existential quantification to make sure |f| works on |a :-> b| for every |a| and |b|.

> data Wrap = forall a b . Wrap (a :-> b)

> instance MuRef Wrap where
>   type DeRef Wrap = MuCont
> 
>   mapDeRef f (Wrap (Action x))        = pure (FAction x)
>   mapDeRef f (Wrap (Edge l r))        = FEdge <$> f (Wrap l) <*> f (Wrap r)
>   mapDeRef f (Wrap (Choice cond l r)) = (FChoice cond) <$> f (Wrap l) <*> f (Wrap r)
>   --mapDeRef f (Wrap (Thread t))        = FThread <$> f (Wrap t)
> 
> explicitSharing = reifyGraph . Wrap
