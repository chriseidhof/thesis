module HAppS where

import HAppS.Server
import Continuations
import Control.Concurrent.MVar
import Control.Monad.Trans
import Text.Printf
import qualified Text.XHtml.Strict.Formlets as F
import qualified Data.ByteString.Lazy.Char8 as B
import Continuations.HAppS

main :: IO ()
main = runServer 8016 [("/", const adder)]

inputInt :: Task Integer
inputInt = form (F.inputInteger Nothing)

adder :: Task ()
adder = do x <- inputInt 
           y <- inputInt
           display "Now I'm going to multiply the integers"
           display $ (printf "%d * %d = %d" x y (x * y) :: String)
