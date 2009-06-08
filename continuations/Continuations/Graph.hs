{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Continuations.Graph (explicitSharing, ExSharing(..)) where

import Data.Reify
import Control.Applicative
import Continuations.Types

data ExSharing u where
      FBox :: (a -> r) -> ExSharing u
      FAction :: (a -> Action b) -> ExSharing u
      FEdge :: u -> u -> ExSharing u
      FChoice :: (a -> Bool) -> u -> u -> ExSharing u

instance (Show u) => Show (ExSharing u) where
  show (FBox x) = "FBox"
  show (FEdge l r) = "FEdge (" ++ show l ++ ") (" ++ show r ++ ")"
  show (FAction x) = "FAction"
  show (FChoice cond l r) = "FChoice cond (" ++ show l ++ ") (" ++ show r ++ ")"


instance Functor ExSharing where
  fmap f (FBox x)    = FBox x
  fmap f (FAction x) = FAction x
  fmap f (FEdge a b) = FEdge (f a) (f b)
  fmap f (FChoice cond l r) = FChoice cond (f l) (f r)

data Cont2 = forall a b . Cont2 (a :-> b)
instance MuRef Cont2 where
  type DeRef Cont2 = ExSharing

  mapDeRef f (Cont2 (Box x))           = pure (FBox x)
  mapDeRef f (Cont2 (Action x))        = pure (FAction x)
  mapDeRef f (Cont2 (Edge l r))        = FEdge <$> f (Cont2 l) <*> f (Cont2 r)
  mapDeRef f (Cont2 (Choice cond l r)) = (FChoice cond) <$> f (Cont2 l) <*> f (Cont2 r)

explicitSharing = reifyGraph . Cont2
