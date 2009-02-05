{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- testing
module Generics.HDBC where

import Generics.Records
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.List (intercalate)
import Control.Applicative

class DbRecord r where
  new     :: IConnection i => i -> r -> IO Integer
  update  :: IConnection i => i -> r -> Integer -> IO ()
  find    :: IConnection i => r -> i -> Integer -> IO (Maybe r)
  findAll :: IConnection i => r -> i            -> IO [(Integer, r)]

instance (Representable a b) => DbRecord a where
  new    = newRep
  update = updateRep
  find r i ix  = (fmap from) <$> findRep r i ix
  findAll r i   = (fmap $ fmap from) <$> findAllRep r i

repKeys :: Rep r ->  [String]
repKeys (Field lbl r)  = [lbl]
repKeys (r1 :*: r2)    = repKeys r1 ++ repKeys r2
repKeys _              = error "Generics.HDBC Implementation error"

repValues :: Rep r -> r -> [SqlValue]
repValues RInt             i = [toSql i]
repValues RInteger         i = [toSql i]
repValues RString          s = [toSql s]
repValues (Field lbl r)    v = repValues r v
repValues (r1 :*: r2) (l, r) = repValues r1 l ++ repValues r2 r

parseDb :: Rep r -> [SqlValue] -> (r, [SqlValue])
parseDb RInt          (x:xs) = (fromSql x, xs)
parseDb RInteger      (x:xs) = (fromSql x, xs)
parseDb RString       (x:xs) = (fromSql x, xs)
parseDb (Field lbl r)    xs  = parseDb r xs
parseDb (r1 :*: r2)      xs  = let (l, xs') = parseDb r1 xs
                                   (r, xs'') = parseDb r2 xs'
                               in  ((l,r), xs'')

-- New records

newQuery t x = "INSERT INTO " ++ t ++ " (" ++ (intercalate ", " keys) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") keys) ++ ") RETURNING id"
  where keys = repKeys (rep x)

newRep conn x = do let query = newQuery "users" x
                       args  = repValues (rep x) (to x)
                   [[SqlInteger i]] <- quickQuery' conn query args
                   commit conn
                   return i

-- Updating records

updateQuery t x = "UPDATE " ++ t ++ " SET (" ++ (intercalate ", " keys) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") keys) ++ ") WHERE id = ?"
  where keys = repKeys (rep x)

updateRep conn x i = do let query = updateQuery "users" x
                            args  = repValues (rep x) (to x) ++ [SqlInteger i]
                        quickQuery' conn query args
                        commit conn

-- Finding single records
findQuery t r = "SELECT " ++ (intercalate ", " keys) ++ " FROM " ++ t ++ " WHERE id = ? LIMIT 1"
  where keys = repKeys r

findRep :: (IConnection c, Representable a r) => a -> c -> Integer -> IO (Maybe r)
findRep x conn i = do let query = findQuery "users" (rep x)
                      result <- quickQuery' conn query [toSql i]
                      commit conn
                      case result of
                           []  -> return Nothing
                           [cols] -> return $ Just $ fst $ parseDb (rep x) cols

-- Finding records
findAllQuery t r = "SELECT " ++ (intercalate ", " keys) ++ " FROM " ++ t
  where keys = ["id"] ++ repKeys r

parseAllDb :: Rep r -> [SqlValue] -> ((Integer, r), [SqlValue])
parseAllDb r (x:xs) = let (result, rest) = parseDb r xs
                      in  ((fromSql x, result), rest)

findAllRep :: (IConnection c, Representable a r) => a -> c -> IO [(Integer, r)]
findAllRep x conn = do let query = findAllQuery "users" (rep x)
                       result <- quickQuery conn query []
                       commit conn
                       return $ map (fst . parseAllDb (rep x)) result
