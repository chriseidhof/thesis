\begin{spec}
data BasilDBState entities rels

type BasilDB entities rels a =   ERModel entities rels 
                             =>  ST.State (BasilDBState entities rels) a

find :: Ref entities entity -> BasilDB entities rels (Maybe entity)

new  ::  Ix entities entity
     ->  entity 
     ->  PList entities entity (InitialValues entities entity rels rels) rels 
     ->  BasilDB entities rels (Ref entities entity)

findAll :: Ix entities entity -> BasilDB entities rels [(Ref entities entity, entity)]

findRels  ::  ( cTarget  ~ TargetCardinality dir rel
              , tTarget  ~ TargetType        dir rel
              , source   ~ SourceType        dir rel
              , rel ~ (Rel entities c1 l c2 r)
              )
          =>  Dir dir
          ->  Ix rels rel
          ->  Ref entities source
          ->  BasilDB entities rels (Maybe (Value entities cTarget tTarget))

update  :: Ref entities row -> row -> BasilDB entities rels ()

delete  :: Ref entities row -> BasilDB entities rels ()

runBasilDB  ::  (  ToSchema entities
                ,  ERModel entities rels
                ,  AddRelationship rels entities tables
                )
            =>  BasilDB entities rels a -> String -> IO a
\end{spec}
