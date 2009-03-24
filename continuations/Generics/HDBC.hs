{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.HDBC where

import Generics.Records
import Database.HDBC
import Data.List (intercalate)
import Control.Applicative
import Data.Char (toLower)

data Options = Options {wher :: [String], order :: [String]}

defaultOptions = Options [] []

class DbRecord r where
  new     :: IConnection i => i -> r -> IO Integer
  update  :: IConnection i => i -> r -> Integer  -> IO ()
  find    :: IConnection i => r -> i -> Integer  -> IO (Maybe r)
  findAll :: IConnection i => r -> i -> Options  -> IO [(Integer, r)]

instance Representable a => DbRecord a where
  new    = newRep
  update = updateRep
  find r i ix  = (fmap from) <$> findRep r i ix
  findAll r i w = (fmap $ fmap from) <$> findAllRep r i w

repKeys :: Rep r ->  [String]
repKeys (Field lbl r)  = [lbl]
repKeys (r1 :*: r2)    = repKeys r1 ++ repKeys r2
repKeys (Con _ r)      = repKeys r

repValues :: Rep r -> r -> [SqlValue]
repValues RInt             i = [toSql i]
repValues RInteger         i = [toSql i]
repValues RString          s = [toSql s]
repValues (Field lbl r)    v = repValues r v
repValues (r1 :*: r2) (l, r) = repValues r1 l ++ repValues r2 r
repValues (Con s r)        x = repValues r x

parseDb :: Rep r -> [SqlValue] -> (r, [SqlValue])
parseDb RInt          (x:xs) = (fromSql x, xs)
parseDb RInteger      (x:xs) = (fromSql x, xs)
parseDb RString       (x:xs) = (fromSql x, xs)
parseDb (Field lbl r)    xs  = parseDb r xs
parseDb (r1 :*: r2)      xs  = let (l, xs') = parseDb r1 xs
                                   (r, xs'') = parseDb r2 xs'
                               in  ((l,r), xs'')
parseDb (Con lbl r)      xs  = parseDb r xs

-- New records

newQuery x = "INSERT INTO " ++ (tableName $ rep x) ++ " (" ++ (intercalate ", " keys) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") keys) ++ ")"
  where keys = repKeys (rep x)

newRep conn x = do let query = newQuery x
                       args  = repValues (rep x) (to x)
                   quickQuery' conn query args
                   [[i]] <- quickQuery' conn "SELECT last_insert_rowid() AS [ID]" []
                   --commit conn
                   return $ fromSql i

-- Updating records

updateQuery x = "UPDATE " ++ (tableName $ rep x) ++ " SET (" ++ (intercalate ", " keys) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") keys) ++ ") WHERE id = ?"
  where keys = repKeys (rep x)

updateRep conn x i = do let repr  = rep x
                            query = updateQuery x
                            args  = repValues repr (to x) ++ [SqlInteger i]
                        quickQuery' conn query args
                        return ()
                        --commit conn

-- Finding single records
findQuery r = "SELECT " ++ (intercalate ", " keys) ++ " FROM " ++ (tableName r) ++ " WHERE id = ? LIMIT 1"
  where keys = repKeys r

findRep :: (IConnection c, Representable a) => a -> c -> Integer -> IO (Maybe (PF a))
findRep x conn i = do let repr  = rep x
                          query = findQuery repr
                      result <- quickQuery' conn query [toSql i]
                      --commit conn
                      case result of
                           []  -> return Nothing
                           [cols] -> return $ Just $ fst $ parseDb (rep x) cols

-- Finding records
findAllQuery t r w = "SELECT " ++ (intercalate ", " keys) ++ " FROM " ++ t ++ whereClause (wher w) ++ orderClause (order w)
  where keys = ["id"] ++ repKeys r
        whereClause [] = ""
        whereClause ls = " WHERE " ++ intercalate " AND " (map (\x -> "(" ++ x ++ ")")  ls)
        orderClause [] = ""
        orderClause ls = " ORDER BY " ++ intercalate "," ls

parseAllDb :: Rep r -> [SqlValue] -> ((Integer, r), [SqlValue])
parseAllDb r (x:xs) = let (result, rest) = parseDb r xs
                      in  ((fromSql x, result), rest)

findAllRep :: (IConnection c, Representable a) => a -> c -> Options -> IO [(Integer, (PF a))]
findAllRep x conn w = do let query = findAllQuery (tableName (rep x)) (rep x) w
                         result <- quickQuery conn query []
                         return $ map (fst . parseAllDb (rep x)) result

-- Utility functions
tableName :: Rep a -> String
tableName (Con s _) = (map toLower s) ++ "s"
tableName _         = error "Couldn't produce the table-name"
