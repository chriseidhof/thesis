{-# LANGUAGE OverlappingInstances #-}
module Samples.User (xmlTask, findUser, register) where

import Continuations
import Continuations.Types
import Generics.Views
import Generics.HDBC (defaultOptions)
import Generics.Xml
import Model
import Text.XHtml.Strict ((+++))
import Text.XML.Light.Output (ppTopElement)
import qualified Text.XHtml.Strict as X

xmlTask = startTask "xml" $ do
  user <- register
  display "Now you have a chance to edit the user again"
  user' <- gEdit user
  display (ppTopElement $ xml user')
  wrap (("Documentation for the user type: " +++ X.br) +++ ) $ gDoc (undefined :: User)

findUser c = startTask "findUser" $ do
  users <- gFindAll c defaultOptions
  display (table (users :: [(Integer, User)]))
  i <- wrap ("Enter the users id" +++) input
  u <- gFind c i
  display $ maybe (X.toHtml "Not Found") view (u :: Maybe User)

register :: Task User
register = wrap (\h -> "Give your user details" +++ h) gInput
