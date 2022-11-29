{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions

data TY = TNum
          | TBool
          | TY TY
            deriving (Show,Eq)

data T where
  Num :: Int -> T
  Boolean :: Bool -> T
  Id :: String -> T
  Plus :: T -> T -> T
  Minus :: T -> T -> T
  Mult :: T -> T -> T
  Div :: T -> T -> T
  Lambda :: String -> T -> T
  App :: T -> T -> T
  Bind :: String -> T -> T -> T
  If :: T -> T -> T -> T
  And :: T -> T -> T
  Or :: T -> T -> T
  Leq :: T -> T -> T
  IsZero :: T -> T
  deriving (Show,Eq)

type Env = [(String,T)]
type Cont = [(String,T)]

