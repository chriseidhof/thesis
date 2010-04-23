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

In this section we will store entities in an internal datatype. We will show how
to construct such a datatype for each entity in our model. We will also define a
datatype that we can use to refer to entities. 

To store entities, we use a |Map| for each entity datatype. The |Map| uses integers
as keys, and the entities as values. When we create an
entity, the integer key is returned as a reference to that entity.
However, an integer value does not indicate the type of the reference. For
example: does the integer |5| refer to a |Person| or a |Compiler| entity?
Therefore, we create the a datatype for entity references.
It is indexed with |a|, which is the type of the entity it refers
to. It is also indexed with |entities|, the type-level list of all entities in
our ER model. To guarantee that the entity |a| is in the list of entities, we
include a typed reference to the entity in the list |entities|:

\label{sec:dataref}

> data Ref entities a where
>   Ref { pr :: Ix entities a, pKey :: Int } ::  Ref entities a

For an entity type |a| we create a map with |Int| as the keys, and |a| as the
values.

> newtype TypeCache a = TypeCache {_cached :: M.Map Int a} deriving Show

If we look at the list of entities in the ER model for compilers, we need a
|TypeCache| structure for each entity in our model:

> type CompilerModel = Compiler :*: Person :*: Release :*: Nil

To do this, we use the type-level function |TMap| from our |HList| library,
which takes a type-level list (such as |CompilerModel|), and wraps each element
in a container type |f|. For example, the result of |TMap TypeCache
CompilerModel| will be |TypeCache Compiler :*: TypeCache Person :*: TypeCache
Release :*: Nil|. The function |TMap| is defined as a type family:

> type family    TMap  (f :: * -> *)  ls        ::  *
> type instance  TMap  f              Nil        =  Nil
> type instance  TMap  f              (a :*: b)  =  f a :*: TMap f b

Now we can write down the type of our storage, which is a heterogenous list
containing a |TypeCache| for each entity in our ER model:

> type EntityStorage entities = HList (TMap TypeCache entities)

To lookup a |TypeCache| for an entity type, we can use the |lookupMapHList|
function, which is defined in the |HList| library and has the following type:

> lookupMapHList :: Ix entities a -> HList (TMap f entities) -> f a

We can define |lookupEntity| in terms of |lookupMapHList|. It looks up the
|TypeCache|, unwraps the |newtype| and looks up the integer key of the entity in
the |Map|.

> lookupEntity :: Ref entities a -> EntityStorage entities -> Maybe a
> lookupEntity (Ref ix key) storage = M.lookup key (_cached $ lookupMapHList ix storage)

To construct an empty |Map| for each entity, we use a |Witnesses| type. The
|Witnesses| type is constructed mechanically, and provides a |WCons| for each
value in a type-level list. It is useful because it allows us to define the
function |emptyState| recursively by deconstructing the |Witnesses| type.

> emptyState :: Witnesses entities entities -> EntityStorage entities
> emptyState WNil         = Nil
> emptyState (WCons _ xs) = TypeCache M.empty .*. emptyState xs

Our next task is to create entities and add them to the storage. For this, we
need to update the corresponding |EntityMap| for an entity. The function looks
up the right |TypeCache| structure and inserts the entity.

> newEntity  ::  Ref entities a 
>            ->  a 
>            ->  (EntityStorage entities -> EntityStorage entities)
> newEntity (Ref ix key) ent = modTList (insertEntity key ent) ix
>  where insertEntity key ent (TypeCache m) = TypeCache (M.insert key ent m)

The function |modTList| is defined in the takes a modifier function and an index, and modifies the given |HList|.
It has the following type:

> modTList    ::  (f ix -> f ix)
>             ->  Ix entities ix
>             ->  HList (TMap f entities)
>             ->  HList (TMap f entities)

We have now created a structure for storing entities in an ER model. The
structure creates a |Map| for each entity in the list of entities, and we have
provided functions to create new entities, to find entities and to create an
empty datastructure.
