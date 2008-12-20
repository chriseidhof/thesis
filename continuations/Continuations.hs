{-# LANGUAGE GADTs #-}

module Continuations where

-- TODO: restructure the Page expression so you have only Endpoints on the left of a Step
data Page a where
  Single :: PageElem a -> Page a
  Step     :: Page a -> (a -> Page b) -> Page b

data PageElem a where
  Const  :: a -> PageElem a
  Form   :: (FormData -> a) -> PageElem a
  ShowIt :: String -> PageElem ()


type FormData = [(String, String)]
type Env      = [(String, Page ())]

instance (Show a) => Show (PageElem a) where
  show (ShowIt text) = "ShowIt: " ++ text
  show (Const x)     = "Const: " ++ (show x)
  show (Form x)      = "Form"

instance (Show a) => Show (Page a) where
  show (Single x) = "Endpoint: " ++ show x
  show (Step _ _)   = "Partial application"

instance Monad Page where
  return = Single . Const
  (>>=)  = Step

form = Single . Form

display :: Show a => a -> Page ()
display =  Single . ShowIt . show

eval :: PageElem a -> FormData -> a
eval (Const x)   e  = x
eval (Form f)    e  = f e
eval (ShowIt s)  e  = ()

run :: Env -> String -> FormData -> (String, Env)
run env page post = case lookup page env of
                    Nothing -> error "Page not found"
                    Just  x -> case x of
                                    (Single a)  -> (show (Single a), env)
                                    (Step (Single f) cont) -> ("papp", (show (length env), cont (eval f post)):env)
