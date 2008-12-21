module Sample where

import Continuations
import Data.Maybe (fromJust)

adder :: Task ()
adder = do x <- inputInt 
           y <- inputInt
           display (show $ x + y)

inputInt :: Task Int
inputInt = form (undefined, read . fromJust . lookup "x") 

link :: String -> Task ()
link x = form (undefined, const ())

inputString = form (undefined, read . fromJust . lookup "y")

initialEnv = [("/", adder)]

test = do let (text, env) = run initialEnv "/" [("x","5")]
          print text
          print env
          let (text', env') =  run env "1" [("x", "7")]
          print text'
          print env'

arcChallenge :: Task ()
arcChallenge = do name <- inputString
                  link "Click here"
                  display ("Hello, " ++ name)
