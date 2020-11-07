{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad()

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

evalDynFAE :: Env -> FAE -> (Maybe FAE)
evalDynFAE _ (Num n) = return (Num n)
evalDynFAE e (Plus l r) = do
                            (Num l') <- evalDynFAE e l;
                            (Num r') <- evalDynFAE e r;
                            return (Num (l' + r'))
evalDynFAE e (Minus l r) = do
                             (Num l') <- evalDynFAE e l;
                             (Num r') <- evalDynFAE e r;
                             if(r' < l') then return (Num (l' - r')) else Nothing
evalDynFAE _ (Lambda i b) = return (Lambda i b)
evalDynFAE e (App f a) = do
                            a' <- evalDynFAE e a;
                            (Lambda i s) <- (evalDynFAE e f);
                            (evalDynFAE ((i,a'):e) s)
evalDynFAE e (Id s) = lookup s e


data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)
  
type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> (Maybe FAEValue)
evalStatFAE _ (Num n) = return (NumV n)
evalStatFAE e (Plus l r) = do
                              (NumV l') <- evalStatFAE e l;
                              (NumV r') <- evalStatFAE e r;
                              return (NumV (l' + r'))                           
evalStatFAE e (Minus l r) = do
                              (NumV l') <- evalStatFAE e l;
                              (NumV r') <- evalStatFAE e r;
                              if(r' < l') then return (NumV (l' - r')) else Nothing                          
evalStatFAE e (Lambda i b) = return (ClosureV i b e)
evalStatFAE e (App f a) = do
                            a' <- evalStatFAE e a;
                            (ClosureV i b e') <- evalStatFAE e f;
                            (evalStatFAE ((i,a'):e') b)                           
evalStatFAE e (Id s) = lookup s e


-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE (NumD n) = (Num n)
elabFBAE (PlusD l r) = Plus (elabFBAE l) (elabFBAE r)
elabFBAE (MinusD l r) = Minus (elabFBAE l) (elabFBAE r)
elabFBAE (LambdaD i b) = Lambda i (elabFBAE b)
elabFBAE (AppD f a) = App (elabFBAE f) (elabFBAE a)
elabFBAE (BindD i a b) = (App (Lambda i (elabFBAE b)) (elabFBAE a))
elabFBAE (IdD s) = Id s



evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
evalFBAE e t = evalStatFAE e (elabFBAE t)

-- FBAEC AST and Type Definitions

data FBAEC where
  NumE :: Int -> FBAEC
  PlusE :: FBAEC -> FBAEC -> FBAEC
  MinusE :: FBAEC -> FBAEC -> FBAEC
  TrueE :: FBAEC
  FalseE :: FBAEC
  AndE :: FBAEC -> FBAEC -> FBAEC
  OrE :: FBAEC -> FBAEC -> FBAEC
  NotE :: FBAEC -> FBAEC
  IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  LambdaE :: String -> FBAEC -> FBAEC
  AppE :: FBAEC -> FBAEC -> FBAEC
  BindE :: String -> FBAEC -> FBAEC -> FBAEC
  IdE :: String -> FBAEC
  deriving (Show,Eq)

elabFBAEC :: FBAEC -> FAE
elabFBAEC (NumE n) = Num n
elabFBAEC (PlusE l r) = Plus (elabFBAEC l) (elabFBAEC r)
elabFBAEC (MinusE l r) = Minus (elabFBAEC l) (elabFBAEC r)
elabFBAEC (TrueE) = Lambda "t" (Lambda "f" (Id "t"))
elabFBAEC (FalseE) = Lambda "t" (Lambda "f" (Id "f"))
elabFBAEC (AndE l r) = App (App (elabFBAEC l) (elabFBAEC r)) (elabFBAEC FalseE)
elabFBAEC (OrE l r) = App (App (elabFBAEC l) (elabFBAEC TrueE)) (elabFBAEC r)
elabFBAEC (NotE b) = App (App (elabFBAEC b) (elabFBAEC FalseE)) (elabFBAEC TrueE)
elabFBAEC (IfE c t e) = if((elabFBAEC c) == (elabFBAEC TrueE)) then (elabFBAEC t) else (elabFBAEC e)
elabFBAEC (LambdaE i b) = Lambda i (elabFBAEC b)
elabFBAEC (AppE f a) = App (elabFBAEC f) ( elabFBAEC a)
elabFBAEC (BindE i a b) = App (Lambda i (elabFBAEC b)) (elabFBAEC a)
elabFBAEC (IdE s) = Id s

evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
evalFBAEC e t = evalStatFAE e (elabFBAEC t)


