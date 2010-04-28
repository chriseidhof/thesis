\begin{spec}
data BasilState entities rels

type Basil entities rels a  =   ERModel entities rels 
                            =>  ST.State (BasilState entities rels) a

find :: Ref entities entity -> Basil entities rels (Maybe entity)

new  ::  Ix entities entity
     ->  entity 
     ->  PList entities entity (InitialValues entities entity rels rels) rels 
     ->  Basil entities rels (Ref entities entity)



findAll :: Ix entities entity -> Basil entities rels [(Ref entities entity, entity)]

query  ::  Ix entities entity 
       ->  Expr entity Bool 
       ->  Basil entities rels [(Ref entities entity, entity)]

attr :: Ref entities entity -> (entity :-> att) -> Basil entities rels att

findRels  ::  ( cTarget  ~ TargetCardinality dir rel
              , tTarget  ~ TargetType        dir rel
              , source   ~ SourceType        dir rel
              , rel ~ (Rel entities c1 l c2 r)
              )
          =>  Dir dir
          ->  Ix rels rel
          ->  Ref entities source
          ->  Basil entities rels (Maybe (Value entities cTarget tTarget))

runBasil  ::  ERModel entities rels
          =>  Basil entities rels a 
          ->  (a, BasilState entities rels)

emptyBasilState  ::  ERModel entities rels 
                 =>  BasilState entities rels

contBasil  ::  ERModel entities rels
           =>  Basil entities rels a 
           ->  BasilState entities rels
           ->  (a, BasilState entities rels)

\end{spec}

Unfortunately, we also need to expose the type-level functions used in the above interface.

\begin{spec}
type family    Value entities cardinality typ :: *
type instance  Value entities One         t   = Ref entities t
type instance  Value entities Many        t   = S.Set (Ref entities t)

data L
data R
data Dir d where
  DL :: Dir L
  DR :: Dir R
type family SourceType dir rel :: *
type instance SourceType L  (Rel entities c1 t1 c2 t2) = t1
type instance SourceType R  (Rel entities c1 t1 c2 t2) = t2

type family TargetType dir rel :: *
type instance TargetType L  (Rel entities c1 t1 c2 t2) = t2
type instance TargetType R  (Rel entities c1 t1 c2 t2) = t1

type family TargetCardinality dir rel :: *
type instance TargetCardinality L  (Rel entities c1 t1 c2 t2) = c2
type instance TargetCardinality R  (Rel entities c1 t1 c2 t2) = c1

type family   InitialValues   entities r rels originalRels :: *
type family   InitialValues'  entities r rels originalRels :: *

type instance InitialValues entities r Nil  o = Nil
type instance InitialValues entities r (Rel entities c1 from Many to :*: xs) o = 
  InitialValues' entities r (Rel entities c1 from Many to :*: xs) o
type instance InitialValues entities r (Rel entities c1 from One  to :*: xs) o = 
  AppendIfTrue  (TypeEq r from) 
                (InitialValue    entities r L (Rel entities c1 from One to) o) 
                (InitialValues'  entities r   (Rel entities c1 from One to :*: xs) o)
type instance InitialValues' entities r (Rel entities One from c1  to :*: xs) o = 
  AppendIfTrue  (TypeEq r to)  
                (InitialValue entities r R (Rel entities One from c1 to) o) 
                (InitialValues entities r xs o)

type instance InitialValues' entities r (Rel entities Many from c1 to :*: xs) o = 
  InitialValues entities r xs o

type InitialValue entities r dir rel rels =  (  Ref entities (TargetType dir rel)
                                             ,  Dir dir
                                             ,  Ix rels rel) 
\end{spec}
