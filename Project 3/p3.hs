{-# LANGUAGE GADTs #-}

-- import Control.Monad

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

-- Substitution Function
subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num n) = (Num n)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Bind i' v' b) = (if (i == i') then (Bind i' (subst i v v') b) else (Bind i' (subst i v v') (subst i v b)))
subst i v (Lambda i' t b) = (Lambda i' t (subst i v b))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (Id i') = (if (i == i') then v else (Id i'))
subst _ _ (Boolean b) = (Boolean b)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero n) = (IsZero (subst i v n))
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (Fix f) = (Fix (subst i v f))

-- Statically scoped eval
         
evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM _ (Num n) = return (NumV n)
evalM e (Plus l r) = do
                      NumV l' <- evalM e l;
                      NumV r' <- evalM e r;
                      return (NumV (l' + r'))
evalM e (Minus l r) = do
                        NumV l' <- evalM e l;
                        NumV r' <- evalM e r;
                        if (l' < r') then Nothing else return (NumV (l' - r'))
evalM e (Mult l r) = do
                      NumV l' <- evalM e l;
                      NumV r' <- evalM e r;
                      return (NumV (l' * r'))
evalM e (Div l r) = do
                      NumV l' <- evalM e l;
                      NumV r' <- evalM e r;
                      if (r' == 0) then Nothing else return (NumV (l' `div` r'))
evalM e (Bind i v b) = do
                        v' <- evalM e v;
                        evalM ((i, v'):e) b
evalM e (Lambda i _ b) = return (ClosureV i b e)
evalM e (App f a) = do 
                     a' <- evalM e a;
                     (ClosureV i b e') <- evalM e f;
                     evalM ((i, a'):e') b
evalM e (Id s) = lookup s e
evalM _ (Boolean b) = return (BooleanV b)
evalM e (And l r) = do
                      BooleanV l' <- evalM e l;
                      BooleanV r' <- evalM e r;
                      return (BooleanV (l' && r'))
evalM e (Or l r) = do
                    BooleanV l' <- evalM e l;
                    BooleanV r' <- evalM e r;
                    return (BooleanV (l' || r'))
evalM e (Leq l r) = do
                      (NumV l') <- evalM e l;
                      (NumV r') <- evalM e r;
                      if (l' <= r') then return (BooleanV True) else return (BooleanV False)
evalM e (IsZero n) = do
                    (NumV n') <- evalM e n;
                    if (n' == 0) then return (BooleanV True) else return (BooleanV False)
evalM e' (If c t e) = do
                      BooleanV c' <- evalM e' c;
                      if (c' == True) then (evalM e' t) else (evalM e' e)
evalM e (Fix f) = do
                    (ClosureV g b e') <- evalM e f;
                    evalM e' (subst g (Fix (Lambda g TNum b)) b)

-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM _ (Num _) = return TNum
typeofM c (Plus l r) = do
                        TNum  <- typeofM c l;
                        TNum  <- typeofM c r;
                        return TNum
typeofM c (Minus l r) = do 
                        TNum  <- typeofM c l;
                        TNum  <- typeofM c r;
                        return TNum
typeofM c (Mult l r) = do
                        TNum  <- typeofM c l;
                        TNum  <- typeofM c r;
                        return TNum
typeofM c (Div l r) = do 
                        TNum  <- typeofM c l;
                        TNum  <- typeofM c r;
                        return TNum
typeofM c (Bind i v b) = do
                          tv <- typeofM c v;
                          typeofM ((i, tv):c) b
typeofM c (Lambda i t b) = do
                            b' <- typeofM ((i, t):c) b;
                            return (t :->: b')
typeofM c (App f a) = do
                        (td :->: tr) <- typeofM c f;
                        ta <- typeofM c a;
                        if (td == ta) then return tr else Nothing

typeofM c (Id i) = lookup i c
typeofM _ (Boolean _) = return TBool
typeofM c (And l r) = do
                        TBool <- typeofM c l;
                        TBool <- typeofM c r;
                        return TBool
typeofM c (Or l r) = do
                        TBool <- typeofM c l;
                        TBool <- typeofM c r;
                        return TBool
typeofM c (Leq l r) = do
                        TBool <- typeofM c l;
                        TBool <- typeofM c r;
                        return TBool
typeofM c (IsZero n) = do
                        TNum <- typeofM c n;
                        return TBool
typeofM c (If c' t e) = do
                        TBool <- typeofM c c';
                        t' <- typeofM c t;
                        e' <- typeofM c e;
                        if (t' == e') then return t' else Nothing
typeofM c (Fix f) = do
                      (d :->: r) <- typeofM c f;
                      return r


-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp exp = do
            typeofM [] exp;
            evalM [] exp

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))
