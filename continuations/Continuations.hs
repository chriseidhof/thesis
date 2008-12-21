{-# LANGUAGE GADTs #-}

module Continuations where

-- TODO: restructure the Page expression so you have only Endpoints on the left of a Step
data Task a where
  Single :: PageElem a -> Task a
  Step   :: Task a -> (a -> Task b) -> Task b

data PageElem a where
  Const  :: a -> PageElem a
  Form   :: (FormData -> a) -> PageElem a
  ShowIt :: String -> PageElem ()


type FormData = [(String, String)]
type Env      = [(String, Task ())]

instance (Show a) => Show (PageElem a) where
  show (ShowIt text) = "ShowIt: " ++ text
  show (Const x)     = "Const: " ++ (show x)
  show (Form x)      = "Form"

instance (Show a) => Show (Task a) where
  show (Single x) = "Endpoint: " ++ show x
  show (Step x y)   = "Partial application, left of the bind is a " ++ (case x of {Single _ -> "single"; _ -> "step"})

instance Monad Task where
  return = Single . Const
  (>>=)  = Step

form = Single . Form

display :: Show a => a -> Task ()
display =  Single . ShowIt . show

eval :: PageElem a -> FormData -> a
eval (Const x)   e  = x
eval (Form f)    e  = f e
eval (ShowIt s)  e  = ()

run :: Env -> String -> FormData -> (String, Env)
run env page post = case lookup page env of
                    Nothing -> error "Task not found"
                    Just  x -> case restructure x of
                                    (Single a)  -> (show (Single a), env)
                                    -- Pattern match cannot fail because of restructure
                                    (Step (Single f) cont) -> ("papp", (show (length env), cont (eval f post)):env)

-- Restructure takes a page and changes the spine so that there is always a Single on the left of the Step
-- We can use the Associativity monad law:  (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-- TODO: prove that this is always the case
restructure :: Task a -> Task a
restructure x@(Single _)    = x
restructure (Step arg cont) = case restructure arg of
                                   (Single _)      -> Step arg cont
                                   Step arg' cont' -> Step arg' (\x -> cont' x >>= cont)
