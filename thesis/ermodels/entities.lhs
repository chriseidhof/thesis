%if False

> {-# LANGUAGE TypeOperators, GADTs, MultiParamTypeClasses,
>              FunctionalDependencies, TypeSynonymInstances,
>              TypeFamilies
>   #-}

> module Entities where

> import Basil.Data.TList hiding (TMap, modTList)
> import Basil.Data.TList4
> import Basil.Core (One, Many, Cardinality (..))
> import Basil.References (Ident)
> import Encoding (Compiler, Release, Person)
> import qualified Data.Map as M


%endif

To store entities, we use a |Map| for each entity datatype. The |Map| uses integers
(the primary keys) as keys, and the entities as values. When we create an
entity, the integer key is returned as a reference to that entity. However,
returning just an integer value is a bit too untyped: we also want to encode the
type that we are dealing with. Therefore, we create the a datatype for
references. It is indexed with |a|, which is the type of the entity it refers
to. To be sure |a| is an entity in our model, we ask for the index |pr| into our
model, and add |entities| as an additional type parameter.

> data Ref entities a where
>   Ref { pr :: Ix entities a, pKey :: Ident } ::  Ref entities a

For a given entity datatype |e| we can create a map. Here, we do not need to
store additional type information, we just store the key.

\begin{spec}
type TypeCache a = TypeCache {_cached :: M.Map Ident a} deriving Show
\end{spec}

We need an |EntityMap| for every entity datatype in our model. Consider the
list of entity datatypes in our ER model:

> type CompilerModel = Compiler :*: Person :*: Release :*: Nil

To store all the |TypeCache| maps for all the entities, we ask for a |HList|
with a |TypeCache| for every element in |model|. The |TMap| function is defined
as following:

> type family    TMap  (f :: * -> *)  ls        ::  *
> type instance  TMap  f              Nil        =  Nil
> type instance  TMap  f              (a :*: b)  =  f a :*: TMap f b

Now we can write down the type of our storage:

> type EntityStorage model = HList (TMap TypeCache model)

However, |TypeCache| is a type synonym, and type synonyms can only be passed to
type-level functions if they are fully applied. Therefore, we need to redefine
|TypeCache| as a |newtype|:

> newtype TypeCache a = TypeCache {_cached :: M.Map Ident a} deriving Show

If we now get an |index| that points into |entities|, we can use the
|lookupMapHList| function from our |HList| library to look up the corresponding
|TypeCache| value. The |lookupMapHList| has the following type:

> lookupMapHList :: Ix entities a -> HList (TMap f entities) -> f a

We can define |lookupEntity| in terms of |lookupMapHList|. It takes an |Ix|
pointing to a type |a|, a reference to |a| and fetches a |Maybe a| value from
the |EntityStorage|.

> lookupEntity :: Ix entities a -> Ref entities a -> EntityStorage entities -> Maybe a
> lookupEntity ix ref storage = M.lookup (pKey ref)  (_cached $ lookupMapHList ix storage)

We now are finally ready to construct our datastructure, containing an empty |Map| for
every entity datatype. To enumerate all entities we use the |Witnesses| type,
which can be found in our library. A witnesses value simply consists of a
|WCons| for every type-level |:*:| and a |WNil| for a type-level |Nil|, and
allows us to construct values from scratch. \todo{Explain witnesses better}

> emptyState :: Witnesses entities entities -> EntityStorage entities
> emptyState WNil         = Nil
> emptyState (WCons _ xs) = TypeCache M.empty .*. emptyState xs

Our next task is to create entities and add them to the storage. For this, we
need to update the corresponding |EntityMap| for an entity. In order to do this,
we need an |Ix| that points to the right entity type, a fresh |Ref| value and
the entity value itself.

> newEntity  ::  Ix entities ent 
>            ->  Ref entities ent 
>            ->  ent 
>            ->  (EntityStorage entities -> EntityStorage entities)
> newEntity ix newRef ent = modTList (insertEntity (pKey newRef) ent) ix
>  where insertEntity key ent (TypeCache m) = TypeCache (M.insert key ent m)

The function |modTList| takes a modifier function and an index, and modifies the given |HList|.
It has the following type:

> modTList    ::  (f ix -> f ix)
>             ->  Ix entities ix
>             ->  HList (TMap f entities)
>             ->  HList (TMap f entities)

We have now seen |lookupEntity| and |newEntity|. Of course, |deleteEntity| and
|modifyEntity| are defined in a similar way.
