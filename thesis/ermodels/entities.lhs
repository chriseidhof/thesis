%if False

> {-# LANGUAGE TypeOperators, GADTs, MultiParamTypeClasses,
>              FunctionalDependencies, TypeSynonymInstances,
>              TypeFamilies
>   #-}

> module Entities where

> import Basil.Data.TList hiding (TMap, modTList)
> import Basil.Data.TList4
> import Basil.Core (One, Many, Cardinality (..))
> import Encoding (Compiler, Release, Person)
> import qualified Data.Map as M


%endif

In this section we store entities in an internal datatype. We show how
to construct such a datatype for each entity in our model. We also define a
datatype that we use to refer to entities in a typed way. 

To store entities, we create a |Map| for each entity datatype. The |Map| uses integers
as keys, and the entities as values. When we create a new
entity, a unique integer key is returned as a reference to that entity.
However, an integer value does not indicate the type of the reference, for
example: does the integer |5| refer to a |Person| or a |Compiler| entity?
Therefore, we create the a datatype representing entity references.
It is indexed with |a|, which is the type of the entity it refers
to. It is also indexed with |entities|, the type-level list of all entities in
our ER model. To guarantee that the entity |a| is in the list of entities, we
include a typed reference to the entity in the list |entities|. This could
possibly be achieved using a class constraint, but having the reference
available is handy for pattern-matching.
The |Rel| datatype is only used in the interface: internally, we use plain |Int|
values as keys, for efficiency.

\label{sec:dataref}

> data Ref entities a where
>   Ref { pr :: Ix entities a, pKey :: Int } ::  Ref entities a

For an entity type |a| we create a map with |Int| values as keys, and |a| as 
values.

> newtype EntityCache a = EntityCache {_cached :: M.Map Int a} deriving Show

For example, if we look at the list of entities in the ER model for compilers, we need a
|EntityCache| structure for each entity in our model:

> type CompilerModel = Compiler :*: Person :*: Release :*: Nil

To do this, we use the type-level function |TMap| from our |HList| library,
which takes a type-level list (such as |CompilerModel|), and wraps each element
in a container type |f|. For example, the result of |TMap EntityCache
CompilerModel| will be |EntityCache Compiler :*: EntityCache Person :*: EntityCache
Release :*: Nil|. The function |TMap| is defined using a type family:

> type family    TMap  (f :: * -> *)  ls         ::  *
> type instance  TMap  f              Nil        =   Nil
> type instance  TMap  f              (a :*: b)  =   f a :*: TMap f b

We denote the type of our data storage for entities, which is a heterogenous list
containing a |EntityCache| for each entity in our ER model:

> type EntityStorage entities = HList (TMap EntityCache entities)

To locate the |EntityCache| for an entity type, we use the |lookupMapHList|
function, which is defined in the |HList| library with the following type:

> lookupMapHList :: Ix entities a -> HList (TMap f entities) -> f a

We define |lookupEntity| in terms of |lookupMapHList|. It looks up the
|EntityCache| containing the entity type we are looking for, unwraps the
|newtype| and looks up the integer key of the entity in the |Map|.

> lookupEntity :: Ref entities a -> EntityStorage entities -> Maybe a
> lookupEntity (Ref ix key) storage = M.lookup key (_cached $ lookupMapHList ix storage)

To construct an empty |Map| for each entity, we use a |Witnesses| type. The
|Witnesses| type is constructed mechanically, and provides a |WCons| for each
value in a type-level list. It is useful because it allows us to define the
function |emptyState| recursively by deconstructing the |Witnesses| type.

> emptyState :: Witnesses entities entities -> EntityStorage entities
> emptyState WNil          = Nil
> emptyState (WCons _ xs)  = EntityCache M.empty .*. emptyState xs

To create entities and add them to the storage, we update the corresponding
|EntityMap| for an entity:

> newEntity  ::  Ref entities a 
>            ->  a 
>            ->  (EntityStorage entities -> EntityStorage entities)
> newEntity (Ref ix key) ent = modTList (insertEntity key ent) ix
>  where insertEntity key ent (EntityCache m) = EntityCache (M.insert key ent m)

We have now described how to store entities in an ER model. The
data structure that stores entities uses a |Map| for each entity in the list of
entity types. We provide functions to add new entities to the data structure,
and to retrieve entities using a reference. 
