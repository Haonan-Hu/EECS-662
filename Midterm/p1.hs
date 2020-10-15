--Name: Haonan Hu
--ID: 2863545
--Date: Oct 7, 2020

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
evalS (IsZero n) = do
                     (Num n') <- evalS n
                     if (n' == 0) then return (Boolean True) else return (Boolean False)
evalS (If c t e) = do
                     (Boolean c') <- evalS c;
                     if c' then (evalS t) else (evalS e)

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM _ (Num n) = Just (Num n)
evalM e (Plus l r) = do
                     (Num l') <- evalM e l;
                     (Num r') <- evalM e r;
                     return (Num (l' + r'))
evalM e (Minus l r) = do
                      (Num l') <- evalM e l;
                      (Num r') <- evalM e r;
                      if(l' >= r') then return (Num (l' - r')) else Nothing
evalM e (Bind x a b) = do
                       a' <- evalM e a;
                       evalM ((x, a'):e) b
evalM e (Id x) = do
                 v <- lookup x e;
                 return v
evalM _ (Boolean b) = Just (Boolean b)
evalM e (And l r) = do
                    (Boolean l') <- evalM e l;
                    (Boolean r') <- evalM e r;
                    return (Boolean (l' && r'))
evalM e (Leq l r) = do
                    (Num l') <- evalM e l;
                    (Num r') <- evalM e r;
                    if(l' <= r') then return (Boolean True) else return (Boolean False)
evalM e (IsZero n) = do
                     (Num n') <- evalM e n
                     if (n' == 0) then return (Boolean True) else return (Boolean False)
evalM e' (If c t e) = do
                     (Boolean c') <- evalM e' c;
                     if c' then (evalM e' t) else (evalM e' e)

testBBAE :: BBAE -> Bool
testBBAE x = if((evalS x) == (evalM [] x)) then True else False

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM _ (Num _) = return TNum
typeofM _ (Boolean _) = return TBool
typeofM c (Plus l r) = do
                        TNum <- typeofM c l;
                        TNum <- typeofM c r;
                        return TNum
typeofM c (Minus l r) = do
                         TNum <- typeofM c l;
                         TNum <- typeofM c r;
                         return TNum
typeofM c (Bind x a b) = do
                          a' <- typeofM c a;
                          typeofM ((x,a'):c) b
typeofM c (Id x) = do
                    x' <- lookup x c;
                    return x'
typeofM c (And l r) = do
                       TBool <- typeofM c l;
                       TBool <- typeofM c r;
                       return TBool
typeofM c (Leq l r) = do
                       TNum <- typeofM c l;
                       TNum <- typeofM c l;
                       return TBool
typeofM c (IsZero n) = do
                        TNum <- typeofM c n;
                        return TBool
typeofM c (If c' t e) = do
                         TBool <- typeofM c c';
                         t' <- typeofM c t;
                         e' <- typeofM c e;
                         if t' == e' then return t' else Nothing

evalT :: BBAE -> (Maybe BBAE)
evalT exp = do
              typeofM [] exp;
              evalM [] exp;

main = do{
    -- test for evalS
    print (evalS (Bind "x" (Boolean False)  (And (Id "x") (Boolean True))));
    print (evalS (Bind "x" (Num 1)  (Leq (Id "x") (Num 5))));
    print (evalS (Bind "x" (Num 1)  (IsZero (Id "x"))));
    print (evalS (Bind "x" (Num 1)  (IsZero (Id "x"))));
    print (evalS (Bind "x" (Num 1) (If (Boolean True) (Plus (Num 1) (Id "x")) (Id "x") )));
    print (evalS (Bind "x" (Boolean True) (If ((Id "x")) (Num 1) (Num 2))));
    print (evalS (Bind "x" (Boolean True) (If (Id "x") (Num 1) (Num 2) )));

    -- test for evalM
    print (evalM [] (Bind "x" (Boolean False)  (And (Id "x") (Boolean True))));
    print (evalM [] (Bind "x" (Num 1)  (Leq (Id "x") (Num 5))));
    print (evalM [] (Bind "x" (Num 1)  (IsZero (Id "x"))));
    print (evalM [] (Bind "x" (Num 1)  (IsZero (Id "x"))));
    print (evalM [] (Bind "x" (Num 1) (If (Boolean True) (Plus (Num 1) (Id "x")) (Id "x") )));
    print (evalM [] (Bind "x" (Boolean True) (If ((Id "x")) (Num 1) (Num 2))));
    print (evalM [] (Bind "x" (Boolean True) (If (Id "x") (Num 1) (Num 2) )));

    -- test for testBBAE
    print (testBBAE (Bind "x" (Boolean False)  (And (Id "x") (Boolean True))));
    print (testBBAE (Bind "x" (Num 1)  (Leq (Id "x") (Num 5))));
    print (testBBAE (Bind "x" (Num 1)  (IsZero (Id "x"))));
    print (testBBAE (Bind "x" (Num 1)  (IsZero (Id "x"))));
    print (testBBAE (Bind "x" (Num 1) (If (Boolean True) (Plus (Num 1) (Id "x")) (Id "x") )));
    print (testBBAE (Bind "x" (Boolean True) (If ((Id "x")) (Num 1) (Num 2))));
    print (testBBAE (Bind "x" (Boolean True) (If (Id "x") (Num 1) (Num 2) )));
}
