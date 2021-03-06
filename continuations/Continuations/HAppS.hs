module Continuations.HAppS (runServer, createServerPart) where

import HAppS.Server
import Continuations
import Continuations.Types
import Control.Concurrent.MVar
import Control.Monad.Trans
import Text.Printf
import qualified Text.XHtml.Strict.Formlets as F
import qualified Data.ByteString.Lazy.Char8 as B


runServer :: Int -> [StartTask] -> IO ()
runServer p start = do
    let env = map (\(StartTask u t) -> (u, const t)) start
    serverPart <- createServerPart env
    putStrLn $ "Running server at http://127.0.0.1:" ++ show  p
    simpleHTTP (nullConf { port = p }) [serverPart]

createServerPart :: Env -> IO (ServerPart Response)
createServerPart e = do env <- newMVar e
                        return $ ServerPartT $ handle env

handle :: MVar Env -> Request -> Web Response
handle env req = do let contId     = foldr const "/" (rqPaths req)
                        formInputs = map (\(k,v) -> (k, Left $ B.unpack $ inputValue v)) $ rqInputs req
                    e <- liftIO $ takeMVar env
                    liftIO $ print (map fst e)
                    (html, e') <- liftIO $ run e contId formInputs
                    liftIO $ putMVar env e'
                    return $ toResponse html

