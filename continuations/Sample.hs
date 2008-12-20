module Sample where

import Continuations
import Data.Maybe (fromJust)

adder :: Page ()
adder = do x <- inputInt 
           y <- inputInt
           display (x + y)

inputInt :: Page Int
inputInt = form (read . fromJust . lookup "x") 

initialEnv = [("/", adder)]

test = do let (text, env) = run initialEnv "/" [("x","5")]
          print text
          print env
          let (text', env') =  run env "1" [("x", "7")]
          print text'
          print env'
