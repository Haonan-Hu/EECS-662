{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

subst::String -> BBAE -> BBAE -> BBAE
subst x v (Num n) = (Num n)
subst x v (Plus l r) = (Plus (subst x v l) (subst x v r))
subst x v (Minus l r) = (Minus (subst x v l) (subst x v r))
subst x v (Bind x' v' b') = if (x == x') then (Bind x' (subst x v v') b') else (Bind x' (subst x v v') (subst x v b'))
subst x v (Id x') = if (x == x') then v else (Id x')
subst x v (Boolean b) = (Boolean b)
subst x v (And l r) = (And (subst x v l) (subst x v r))
subst x v (Leq l r) = (Leq (subst x v l) (subst x v r))
subst x v (IsZero n) = (IsZero (subst x v n))
subst x v (If c t e) = (If (subst x v c) (subst x v t) (subst x v e))


evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = Just (Num n)
evalS (Plus l r) = do
                     (Num l') <- evalS l;
                     (Num r') <- evalS r;
                     return (Num (l' + r'))
evalS (Minus l r) = do
                      (Num l') <- evalS l;
                      (Num r') <- evalS r;
                      if(l' >= r') then return (Num (l' - r')) else Nothing
evalS (Bind x a b) = do
                       a' <- evalS a;
                       evalS (subst x a' b)
evalS (Id x) = Nothing
evalS (Boolean b) = Just (Boolean b)
evalS (And l r) = do
                    (Boolean l') <- evalS l;
                    (Boolean r') <- evalS r;
                    return (Boolean (l' && r'))
evalS (Leq l r) = do
                    (Num l') <- evalS l;
                    (Num r') <- evalS r;
                    if(l' <= r') then return (Boolean True) else return (Boolean False)
evalS (IsZero n) = if (n == (Num 0)) then return (Boolean True) else return (Boolean False)
evalS (If c t e) = do
                     (Boolean c') <- evalS c;
                     if c' then (evalS t) else (evalS e)

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM _ _ = Nothing

testBBAE :: BBAE -> Bool
testBBAE _ = True

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM _ _ = Nothing

evalT :: BBAE -> (Maybe BBAE)
evalT _ = Nothing
