> {-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls,
>              TypeSynonymInstances, TypeOperators, ScopedTypeVariables #-}
>
> module Routing where
>
> import Generics.Regular
> import Data.Char (toLower)
> import Data.List (intercalate)

From here on we define the library. For now, we use |String|s as our |URL|
types, but this should of course be a proper URL type.

> type URL = [String]
> (</>) :: URL -> URL -> URL
> renderURL  :: URL -> String
> components :: String -> URL

We need two typeclasses: one for the generic functionality, |GToURL|, and one for the
normal datatypes, |ToURL|.

> class GToURL f where
>   gtoURL   :: f a -> URL
>   gfromURL :: URL -> Maybe (f a)

> instance ToURL a => GToURL (K a) where
>   gtoURL (K a) = toURL a
>   gfromURL x   = fmap K (fromURL x)
>
> instance (GToURL f, GToURL g) => GToURL (f :+: g) where
>   gtoURL   (L x) = gtoURL x
>   gtoURL   (R y) = gtoURL y
>   gfromURL s = let urlLeft  = fmap L (gfromURL s)
>                    urlRight = fmap R (gfromURL s)
>                in case urlLeft of
>                     Nothing -> urlRight
>                     x       -> x

> instance GToURL U where
>   gtoURL   U  = []
>   gfromURL [] = Just U
>   gfromURL _  = Nothing

> instance GToURL f => GToURL (S s f) where
>   gtoURL   (S x) = gtoURL x
>   gfromURL x     = fmap S (gfromURL x)

> instance (Constructor c, GToURL f) => GToURL (C c f) where
>   gtoURL c@(C x)  = [lower $ conName c] </> gtoURL x
>   gfromURL (x:xs) = let constr = undefined :: C c f r
>                         name   = conName constr
>                      in if   (lower x == lower name)
>                         then fmap C (gfromURL xs)
>                         else Nothing

> class ToURL a where
>   toURL   :: a -> URL
>   fromURL :: URL -> Maybe a

> instance ToURL String where
>   toURL    x  = [show x]   -- there should be escaping here!!
>   fromURL [x] = Just x
>   fromURL _   = Nothing

> instance ToURL Int where
>   toURL    x  = [show x]
>   fromURL [x] = Just (read x) -- completely unsafe read. what if read fails?
>   fromURL _   = Nothing

Some utility functions:

> lower     = map toLower
> l </> r = l ++ r
> renderURL = intercalate "/"
> components s =  case dropWhile isSlash s of
>                 "" -> []
>                 s' -> w : components s''
>                  where (w, s'') = break isSlash s'
>  where isSlash x = x == '/'


