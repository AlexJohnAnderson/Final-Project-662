{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions

data TY = Num
          | Boolean
          | TY -> TY
            deriving (Show,Eq)

data T = Num  Int 
         | True Boolean
         | False Boolean
         | Id String
         | Plus T T 
         | Minus T T
         | Mult T T
         | Div T T
         | Lambda (Id:TY) in T
         | App T T
         | Bind Id T T 
         | If T T T
         | And T T
         | Or T T
         | Leq T T
         | IsZero T 
           deriving (Show,Eq)

type Env = [(String,TERMLANG)]
type Cont = [(String,TYPELANG)]
