-- The Arc Challenge
module Samples.Arc (arc) where

import Continuations

arc = startTask "arc" $ do 
  name <- input
  (x,y) <- input
  link "click on this link"
  display $ "Hello, " ++ name
  display $ show $ multiply x y

multiply :: Integer -> Integer -> Integer
multiply x y = x * y
