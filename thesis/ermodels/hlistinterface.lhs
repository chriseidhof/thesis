\begin{spec}
infixr 5 :*:
infixr 5 .*.
infixr 5 .**.

data (:*:) a b
data Nil

(.*.) :: a -> HList b -> HList (a :*: b)

(.**.) :: f a -> HList2 f b -> HList2 f (a :*: b)

data HList a where
  Nil  :: HList Nil
  Cons :: a -> HList b -> HList (a :*: b)

data HList2 f a where
  Nil2  :: HList2 f Nil
  Cons2 :: f a -> HList2 f b -> HList2 f (a :*: b)

data Ix phi ix where
  Zero :: Ix (a :*: b) a
  Suc  :: Ix xs a -> Ix (x :*: xs) a

type family   TMap (f :: * -> *) phi :: *
type instance TMap f Nil       = Nil
type instance TMap f (a :*: b) = f a :*: TMap f b

data Witnesses finalEnv env where
  WNil  :: Witnesses finalEnv Nil
  WCons :: Ix finalEnv ix -> Witnesses finalEnv env -> Witnesses finalEnv (ix :*: env)

lookupTList :: Ix phi ix -> HList phi -> ix

lookupMapTList :: Ix phi ix -> HList (TMap f phi) -> f ix

modTList :: (f ix -> f ix)
            -> Ix phi ix
            -> HList (TMap f phi)
            -> HList (TMap f phi)

foldTList :: (forall ix . f ix -> r -> r)
          -> r
          -> HList2 f phi
          -> r

zipHlistWithHList2 :: HList a -> HList2 f a -> HList2 (WithMeta f) a

data WithMeta f a = Combined a (f a)

type family   AppendIfTrue bool x xs :: *
type instance AppendIfTrue True x xs  = x :*: xs
type instance AppendIfTrue False x xs = xs
\end{spec}
