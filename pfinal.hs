{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions

data TY = Num
          | Boolean
          | TY TY
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
         | If T T T
         | Bind Id T T
           deriving (Show,Eq)

data TX = NumX  Int 
         | TrueX Boolean
         | FalseX Boolean
         | IdX String
         | PlusX TX TX 
         | MinusX TX TX
         | MultX TX TX
         | DivX TX TX
         | AndX TX X
         | OrX TX TX
         | LeqX TX TX
         | IsZeroX TX 
         | IfX TX TX TX
         | BindX IdX TX TX
         | PairX TX TX TX 
         | FirstX TX TX
         | LastX  TX TX
           deriving (Show,Eq)

type Env = [(String,TERMLANG)]
type Cont = [(String,TYPELANG)]

--imagine we have booleans added back in to FAE, which means I have if statements

PairX t1 t2 = if x then t1 else t2 --here we have no idea what x is
PairX t1 t2 = lambda x in (if x then t1 else t2)
FirstX t = (t)(true)
LastX t = (t)(false)

-- what's happening in the application
-- FirstX (PairX t1 t2)
-- == (lambda x in (if x then t1 else t2))(true)
-- == if true then t1 else t2
-- == t1