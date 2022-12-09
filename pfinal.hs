{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads
import Control.Monad

type List a = Int -> a
-- AST and Type Definitions
data TY where
  TNum :: TY
  TBool :: TY
  (:->:) :: TY -> TY -> TY
  deriving (Show,Eq)

data T where
  Num :: Int -> T
  Boolean :: Bool -> T
  Id :: String -> T
  Plus :: T -> T -> T
  Minus :: T -> T -> T
  Mult :: T -> T -> T
  Div :: T -> T -> T
  Lambda :: String -> TY -> T -> T
  App :: T -> T -> T
  Bind :: String -> T -> T -> T
  If :: T -> T -> T -> T
  And :: T -> T -> T
  Or :: T -> T -> T
  Leq :: T -> T -> T
  IsZero :: T -> T
  Fix :: T -> T
  Cons :: T -> T -> T -> T
  Initialize :: T -> T -> T
  Head :: T -> T
  Tail :: T -> T
  deriving (Show,Eq)

data TVal where
  NumV :: Int -> TVal
  BooleanV :: Bool -> TVal
  ListV :: Maybe TVal -> TVal
  ClosureV :: String -> T -> Env -> TVal
  deriving (Show,Eq)

type Env = [(String,TVal)]
type Cont = [(String,TY)]


--SUBSTITUTE FUNCTION
subst :: String -> T -> T -> T
subst i v (Num x) = (Num x)
subst i v (Boolean b) = (Boolean b)
subst i v (Id i') = if i == i' then v else (Id i')
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Lambda i' v' b') = (Lambda i' v' (subst i v b'))
subst i v (App f' a') = (App (subst i v f') (subst i v a'))
subst i v (Bind i' v' b') = if i == i' then (Bind i' (subst i v v') b') else (Bind i' (subst i v v') (subst i v b'))
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (And x y) = (And (subst i v x) (subst i v y))
subst i v (Or x y) = (Or (subst i v x) (subst i v y))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero t) = (IsZero (subst i v t))
subst i v (Fix f) = (Fix (subst i v f))


--TYPE INFERENCE FUNCTION
typeof :: Cont -> T -> (Maybe TY)
typeof c (Num x) = if x < 0
                   then Nothing
                   else (return TNum)
typeof c (Boolean b) = return TBool
typeof c (Id i) = (lookup i c)
typeof c (Plus l r) = do { TNum <- typeof c l;
                           TNum <- typeof c r;
                           return TNum }
typeof c (Minus l r) = do { TNum <- typeof c l;
                            TNum <- typeof c r;
                            return TNum }
typeof c (Mult l r) = do { TNum <- typeof c l;
                           TNum <- typeof c r;
                           return TNum }
typeof c (Div l r) =do { TNum <- typeof c l;
                         TNum <- typeof c r;
                         return TNum }
typeof c (Lambda i v b) = do { r <- (typeof ((i,v):c) b); 
                               return (v :->: r) }
typeof c (App f a) = do { a' <- (typeof c a);
                          (d :->: r) <- (typeof c f);
                          if (d == a') then (return r) else (Nothing) }
typeof c (Bind i v b) = do { tv <- typeof c v;
                             typeof ((i,tv):c) b }
typeof c (If c' t e) = do { TBool <- typeof c c';
                            t' <- typeof c t;
                            e' <- typeof c e;
                            if (t' == e') then (return t') else (Nothing) }
typeof c (And x y) = do { TBool <- typeof c x;
                          TBool <- typeof c y;
                          return TBool }
typeof c (Or x y) = do { TBool <- typeof c x;
                         TBool <- typeof c y;
                         return TBool }
typeof c (Leq l r) = do { TNum <- typeof c l;
                          TNum <- typeof c r;
                          return TBool }
typeof c (IsZero t) = do { TNum <- typeof c t;
                           return TBool }
typeof c (Fix f) = do { (d :->: r) <- (typeof c f);
                        return r }
typeof c (Initialize n x) = return TNum
typeof c (Cons x xs n) = return TNum
typeof c (Head xs) = return TNum
typeof c (Tail xs) = return TNum


--EVALUATION
eval :: Env -> T -> (Maybe TVal)
eval e (Num x) = if x < 0 then (Nothing) else (Just (NumV x))
eval e (Boolean b) = Just (BooleanV b)
eval e (Id i) = (lookup i e)
eval e (Plus l r) = do { (NumV l') <- (eval e l);
                         (NumV r') <- (eval e r);
                         return (NumV (l' + r')) }
eval e (Minus l r) = do { (NumV l') <- (eval e l);
                          (NumV r') <- (eval e r);
                          if (l' < r') then (Nothing) else (return (NumV (l' - r'))) }
eval e (Mult l r) = do { (NumV l') <- (eval e l);
                         (NumV r') <- (eval e r);
                         return (NumV (l' * r')) }
eval e (Div l r) = do { (NumV l') <- (eval e l);
                        (NumV r') <- (eval e r);
                        if (r' == 0) then (Nothing) else (return (NumV (l' `div` r'))) }
eval e (Lambda i t b) = Just (ClosureV i b e)
eval e (App f a) = do { (ClosureV i b e') <- (eval e f);
                        a' <- (eval e a);
                        (eval ((i,a'):e') b) }
eval e (Bind i v b) = do { v' <- (eval e v);
                           eval ((i,v'):e) b }
eval e (If c t e') = do { (BooleanV c') <- (eval e c);
                         if c' then (eval e t) else (eval e e') }
eval e (And x y) = do { (BooleanV x') <- (eval e x);
                        (BooleanV y') <- (eval e y);
                        return (BooleanV (x' && y')) }
eval e (Or x y) = do { (BooleanV x') <- (eval e x);
                       (BooleanV y') <- (eval e y);
                       return (BooleanV (x' || y')) }
eval e (Leq l r) = do { (NumV l') <- (eval e l);
                        (NumV r') <- (eval e r);
                        return (BooleanV (l' <= r')) }
eval e (IsZero t) = do { (NumV t') <- (eval e t);
                         return (BooleanV (t' == 0)) }
eval e (Fix f) = do { (ClosureV i b e') <- (eval e f);
                      t <- Just TNum;
                      (eval e' (subst i (Fix (Lambda i t b)) b)) }
eval e (Initialize n xs) = do{ListV xs'<-(eval e xs);
                              NumV n'<-(eval e n);
                              if n' == 0 then Nothing else xs'}
eval e (Cons x xs n) = do{ListV x'<-(eval e x);
                          ListV xs'<-(eval e xs);
                          NumV n'<-(eval e n);
                          if n' == 0 then x' else xs'}
--eval e (Head xs) = do{ListV xs'<-(eval e xs);
--                      return head(xs')}
--eval e (Tail xs) = do{ListV xs'<-(eval e xs);
--                      return tail(xs')}

--INTERPRETER
interp :: T -> (Maybe TVal)
interp a = let env = [] in
           do { typeof env a;
                eval env a; }


-- Simulate infinite lists as functions from Integer
type ListX a = Int -> a

cons :: Int -> ListX Int -> ListX Int
cons x xs n | n == 0    = x
            | otherwise = xs (n-1)

tailF :: ListX a -> ListX a
tailF xs n = xs (n+1)

headF :: ListX a -> ListX a
headF xs n = xs(n-(n-1))