{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions

data TYPELANG = TNum
              | TBool
                deriving (Show,Eq)

data TERMLANG = Num  Int 
              | Plus TERMLANG TERMLANG 
              | Minus TERMLANG TERMLANG
              | Mult TERMLANG TERMLANG
              | Div TERMLANG TERMLANG
              | Boolean Bool
              | And TERMLANG TERMLANG
              | Or TERMLANG TERMLANG
              | Leq TERMLANG TERMLANG
              | IsZero TERMLANG 
              | If TERMLANG TERMLANG TERMLANG
              | Bind String TERMLANG TERMLANG
              | Id String 
                deriving (Show,Eq)
  

type Env = [(String,TERMLANG)]
type Cont = [(String,TYPELANG)]


-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Adding Booleans

-- Exercise 1

subst :: String -> TERMLANG -> TERMLANG -> TERMLANG
subst i v (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Boolean b) = (Boolean b)
subst i v (And x y) = (And (subst i v x) (subst i v y))
subst i v (Or x y) = (Or (subst i v x) (subst i v y))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero t) = (IsZero (subst i v t))
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (Bind i' v' b') = if i == i' then (Bind i' (subst i v v') b') else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i == i' then v else (Id i')


evalS :: TERMLANG -> (Maybe TERMLANG)
evalS (Num x) = if x < 0
                then Nothing
                else (Just (Num x))
evalS (Plus l r) = do { (Num l') <- evalS l;
                        (Num r') <- evalS r;
                        (return (Num (l' + r'))) }
evalS (Minus l r) = do { (Num l') <- evalS l;
                         (Num r') <- evalS r;
                         if ((l' - r') < 0) then Nothing else (return (Num (l' - r'))) }
evalS (Mult l r) = do { (Num l') <- evalS l;
                        (Num r') <- evalS r;
                        (return (Num (l' * r'))) }
evalS (Div l r) = do { (Num l') <- evalS l;
                       (Num r') <- evalS r;
                       if (r' == 0) then Nothing else (return (Num (l' `div` r'))) }
evalS (Boolean b) = (Just (Boolean b))
evalS (And x y) = do { (Boolean x') <- evalS x;
                       (Boolean y') <- evalS y;
                       (return (Boolean (x' && y'))) }
evalS (Or x y) = do { (Boolean x') <- evalS x;
                      (Boolean y') <- evalS y;
                      (return (Boolean (x' || y'))) }
evalS (Leq l r) = do { (Num l') <- evalS l;
                       (Num r') <- evalS r;
                       if (l' <= r') then return (Boolean True) else return (Boolean False) }
evalS (IsZero t) = do { (Num t') <- evalS t;
                        if (t' == 0) then return (Boolean True) else return (Boolean False) }
evalS (If c t e) = do { (Boolean c') <- evalS c;
                        if (c' == (True)) then (evalS t) else (evalS e) }
evalS (Bind i v b) = do { v' <- (evalS v);
                          (evalS (subst i v' b)) }
evalS (Id i) = Nothing


-- Exercise 2
evalM :: Env -> TERMLANG -> (Maybe TERMLANG)
evalM e (Num x) = if x < 0
                  then Nothing
                  else (Just (Num x))
evalM e (Plus l r) = do { (Num l') <- evalM e l;
                          (Num r') <- evalM e r;
                          (return (Num (l' + r'))) }
evalM e (Minus l r) = do { (Num l') <- evalM e l;
                           (Num r') <- evalM e r;
                           if ((l' - r') < 0) then Nothing else (return (Num (l' - r'))) }
evalM e (Mult l r) = do { (Num l') <- evalM e l;
                          (Num r') <- evalM e r;
                          (return (Num (l' * r'))) }
evalM e (Div l r) = do { (Num l') <- evalM e l;
                         (Num r') <- evalM e r;
                         if (r' == 0) then Nothing else (return (Num (l' `div` r'))) }
evalM e (Boolean b) = (Just (Boolean b))
evalM e (And x y) = do { (Boolean x') <- evalM e x;
                         (Boolean y') <- evalM e y;
                         (return (Boolean (x' && y'))) }
evalM e (Or x y) = do { (Boolean x') <- evalM e x;
                        (Boolean y') <- evalM e y;
                        (return (Boolean (x' || y'))) }
evalM e (Leq l r) = do { (Num l') <- evalM e l;
                         (Num r') <- evalM e r;
                         if (l' <= r') then return (Boolean True) else return (Boolean False) }
evalM e (IsZero t) = do { (Num t') <- evalM e t;
                          if (t' == 0) then return (Boolean True) else return (Boolean False) }
evalM e (If c t e') = do { (Boolean c') <- evalM e c;
                           if (c' == (True)) then (evalM e t) else (evalM e e') }
evalM e (Bind i v b) = do { v' <- (evalM e v);
                            (evalM ((i,v'):e) b) }
evalM e (Id i) = (lookup i e)


-- Exercise 3
testEvals :: TERMLANG -> Bool
testEvals e = if evalM [] e == evalS e then True else False

-- Part 2: Type Checking

--Exercise 1
typeofM :: Cont -> TERMLANG -> (Maybe TYPELANG)
typeofM c (Num x) = if x < 0
                    then Nothing
                    else (return TNum)
typeofM c (Plus l r) = do { TNum <- typeofM c l;
                            TNum <- typeofM c r;
                            return TNum }
typeofM c (Minus l r) = do {  TNum <- typeofM c l;
                              TNum <- typeofM c r;
                              return TNum }
typeofM c (Mult l r) = do { TNum <- typeofM c l;
                            TNum <- typeofM c r;
                            return TNum }
typeofM c (Div l r) =do { TNum <- typeofM c l;
                          TNum <- typeofM c r;
                          return TNum }
typeofM c (Boolean b) = return TBool
typeofM c (And x y) = do {  TBool <- typeofM c x;
                            TBool <- typeofM c y;
                            return TBool }
typeofM c (Or x y) = do { TBool <- typeofM c x;
                          TBool <- typeofM c y;
                          return TBool }
typeofM c (Leq l r) = do {  TNum <- typeofM c l;
                            TNum <- typeofM c r;
                            return TBool }
typeofM c (IsZero t) = do { TNum <- typeofM c t;
                            return TBool }
typeofM c (If c' t e) = do {  TBool <- typeofM c c';
                              t' <- typeofM c t;
                              e' <- typeofM c e;
                              if t' == e' then return t' else Nothing }
typeofM c (Bind i v b) = do { tv <- typeofM c v;
                              typeofM ((i,tv):c) b }
typeofM c (Id i) = (lookup i c)

--Exercise 2
evalT :: TERMLANG -> (Maybe TERMLANG)
evalT e = do {typeofM [] e;
              evalM [] e}