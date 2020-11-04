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
elabFBAE _ = (Num (-1))

evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
evalFBAE _ _ = Nothing

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
elabFBAEC _ = (Num (-1))

evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
evalFBAEC _ _ = Nothing


