{-# LANGUAGE FlexibleInstances #-} -- for debugging
module Continuations.Salvia (runServer) where

import Network.Salvia.Httpd
import qualified Network.Protocol.Http as H
import qualified Network.Protocol.Uri as U
import Network.Salvia.Handlers.Session hiding (start)
import Network.Salvia.Handlers.Default
import Data.Record.Label
import Continuations
import Continuations.Types
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Trans
import Text.Printf
import qualified Text.XHtml.Strict.Formlets as F
import qualified Text.XHtml.Strict as X
import qualified Data.ByteString.Lazy.Char8 as B


runServer :: Int -> [StartTask] -> IO ()
runServer p startTasks = do
    let env = map (\(StartTask u t) -> (u, const $ return t)) startTasks
    count <- atomically $ newTVar 0
    sessions <- mkSessions :: IO (Sessions (Env))
    cfg <- defaultConfig
    start cfg  {listenPort = fromIntegral p} $ hDefault count sessions (handler env)

instance Show (FormData -> IO (Task ())) where
  show = const "continuation"

handler :: Env -> TVar (Session Env) -> Handler ()
handler defaultEnv sess = do
  env' <- lift $ atomically $ readTVar sess
  let env = maybe defaultEnv id (payload env')
  path <- getM (U.path % H.uri % request)
  if path == "/favicon.ico" then return () else do
    let contId     = if path == "/" then "/" else tail path
    lift $ print contId
    lift $ print ("Environment", map fst env)
    params <- uriEncodedPostParamsUTF8
    let formInputs = map (\(a,b) -> (a, Left $ maybe "" id b)) (maybe [] id params)
    (html, e') <- lift $ run env contId formInputs
    lift $ atomically $ writeTVar sess env' {payload = Just e'}
    enterM response $ do
      setM H.status H.OK
      setM H.contentType ("text/html", Nothing)
    sendStr $ X.renderHtml html
