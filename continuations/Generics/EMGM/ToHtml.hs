{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Generics.EMGM.ToHtml where

import Generics.EMGM hiding (show, Show)
import Data.Char (toUpper)
import Text.XHtml.Strict hiding (address)

--------------------------------------------------------------------------------

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

labelToHtml lbl = label << (capitalize lbl ++ ": ")

--------------------------------------------------------------------------------

type Labels = [String]

newtype ToHtml a = ToHtml { selToHtml :: Labels -> a -> Html }

rconstantToHtml :: (Show a) => Labels -> a -> Html
rconstantToHtml lbls x =
  case lbls of
    []     -> output x
    lbl:[] -> labelToHtml lbl +++ output x
    other  -> error $ "Unexpected labels in rconstant: " ++ show other
  where
    output = toHtml . show

rsumToHtml ra rb lbls x =
  case x of
    L a -> selToHtml ra lbls a
    R b -> selToHtml rb lbls b

rprodToHtml ra rb lbls (a :*: b) =
  case lbls of
    [] ->
      selToHtml ra [] a
      +++ br +++
      selToHtml rb [] b
    l:ls ->
      labelToHtml l +++ selToHtml ra [] a
      +++ br +++
      selToHtml rb ls b

rconToHtml cd ra _ =
  selToHtml ra (conLabels cd)

rtypeToHtml ep ra lbls a =
  case lbls of
    []     -> selToHtml ra [] (from ep a)
    lbl:[] -> labelToHtml lbl +++ selToHtml ra [] (from ep a)
    other  -> error $ "Unexpected labels in rtype: " ++ show other

instance Generic ToHtml where
  rconstant      = ToHtml rconstantToHtml
  rsum     ra rb = ToHtml (rsumToHtml ra rb)
  rprod    ra rb = ToHtml (rprodToHtml ra rb)
  rcon  cd ra    = ToHtml (rconToHtml cd ra)
  rtype ep ra    = ToHtml (rtypeToHtml ep ra)

instance Rep ToHtml String where
  rep = ToHtml (\_ -> toHtml)

gToHtml :: (Rep ToHtml a) => a -> Html
gToHtml = selToHtml rep []
