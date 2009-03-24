{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Generics.EMGM.Form where

import Generics.EMGM hiding (show, Show, read, Read)
import Control.Monad (ap)
import Control.Monad.Identity
import Control.Applicative
import Control.Applicative.Error
import Data.Char (toUpper)
import Text.XHtml.Strict hiding (address, label)
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F

--------------------------------------------------------------------------------

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

labelToHtml lbl = X.label << (capitalize lbl ++ ": ")

--------------------------------------------------------------------------------

type Labels = [String]

type Form a = F.XHtmlForm Identity a

instance Applicative Identity where
  pure  = return
  (<*>) = ap

newtype GForm a = GForm { selToForm :: Labels -> Form a }

label l f = F.plug (labelToHtml l +++) f

rconstantToForm :: (Read a) => Labels -> Form a
rconstantToForm lbls = 
  case lbls of
    []     -> form
    lbl:[] -> label lbl form
    other  -> error $ "Unexpected labels in rconstant: " ++ show other
  where
    form = F.input Nothing `F.check` maybeRead' "Could not parse"

rsumToForm ra rb lbls = L <$> selToForm ra lbls

rprodToForm ra rb (l:ls) = (:*:) <$> selToForm ra [l] <*> selToForm rb ls

rconToForm cd ra _ = selToForm ra (conLabels cd)

rtypeToForm ep ra lbls =
  let frm = to ep <$> selToForm ra [] in
  case lbls of
    []    -> error $ "No labels available in rtype"
    [lbl] -> label lbl frm
    other -> error $ "Unexpected labels in rtype: " ++ show other

instance Generic GForm where
  rconstant      = GForm rconstantToForm
  rsum     ra rb = GForm (rsumToForm ra rb)
  rprod    ra rb = GForm (rprodToForm ra rb)
  rcon  cd ra    = GForm (rconToForm cd ra)
  rtype ep ra    = GForm (rtypeToForm ep ra)

instance Rep GForm String where
  rep = GForm (\[l] -> label l $ F.input Nothing)

gForm :: (Rep GForm a) => Form a
gForm = selToForm rep []
