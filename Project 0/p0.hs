--Haonan Hu
--2863545
--13 Sep, 2020
{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad()
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--for quickcheck, so Dr.Alexander, you might need to install quickcheck package from cabal
--but you 99.99% chance already have it, just a quick reminder, just in case code cant compile
--because of it
import Test.QuickCheck()

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }

lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

--print
pprint :: AE -> String
pprint (Num n ) = show n
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ " * " ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ " / " ++ pprint m ++ ")"
pprint (If0 c t e) = "(if0 " ++ pprint c ++ " then " ++ pprint t ++ " else " ++ pprint e ++ ")"

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

evalAE :: AE -> Int
evalAE (Num n) = n
evalAE (Plus l r) = (evalAE l) + (evalAE r)
evalAE (Minus l r) = let r' = (evalAE l) - (evalAE r) in
                      if(r' < 0) then error "!" else r'
evalAE (Mult l r) = (evalAE l) * (evalAE r)
evalAE (Div l r) = let r' = (evalAE r) in
                    if(r' == 0) then error "!!" else (evalAE l) `div` r'
evalAE (If0 c t e) = if((evalAE c) == 0) then (evalAE t) else (evalAE e)

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num n) = Just n
evalAEMaybe (Plus l r) = case (evalAEMaybe l) of
                            Nothing -> Nothing
                            Just l' -> case (evalAEMaybe r) of
                                          Nothing -> Nothing
                                          Just r' -> Just (l' + r')
evalAEMaybe (Minus l r) = case (evalAEMaybe l) of
                            Nothing -> Nothing
                            Just l' -> case (evalAEMaybe r) of
                                          Nothing -> Nothing
                                          Just r' -> if r' <= l' then Just (l' - r') else Nothing
evalAEMaybe (Mult l r) = case (evalAEMaybe l) of
                            Nothing -> Nothing
                            Just l' -> case (evalAEMaybe r) of
                                          Nothing -> Nothing
                                          Just r' -> Just (l' * r')
evalAEMaybe (Div l r) = case (evalAEMaybe l) of
                            Nothing -> Nothing
                            Just l' -> case (evalAEMaybe r) of
                                          Nothing -> Nothing
                                          Just r' -> if r' /= 0 then Just (l' `div` r') else Nothing
evalAEMaybe (If0 c t e) = if((evalAEMaybe c) == evalAEMaybe (Num 0)) then (evalAEMaybe t) else (evalAEMaybe e)

evalM :: AE -> Maybe Int
evalM (Num n) = do
                x <- Just n;
                return x
evalM (Plus l r) = do
                   l' <- evalM l;
                   r' <- evalM r;
                   return (l' + r')
evalM (Minus l r) = do
                    l' <- evalM l;
                    r' <- evalM r;
                    if(r' <= l') then return (l' - r') else Nothing
evalM (Mult l r) = do
                   l' <- evalM l;
                   r' <- evalM r;
                   return (l' * r')
evalM (Div l r) = do
                  l' <- evalM l;
                  r' <- evalM r;
                  if(r' /= 0) then return (l' `div` r') else Nothing
evalM (If0 c t e) = do
                    x <- if((evalM c) == evalM (Num 0)) then (evalM t) else (evalM e)
                    return x


interpAE :: String -> Maybe Int
interpAE x = evalM (parseAE x)

--Testing stuff
instance Arbitrary AE where
  arbitrary = sized $ \n -> genAE (rem n 10)

genNum :: Gen AE
genNum = do
          t <- choose (0,100)
          return (Num t)

genPlus :: Int -> Gen AE
genPlus n = do
              s <- genAE n
              t <- genAE n
              return (Plus s t)

genMinus :: Int -> Gen AE
genMinus n = do
              s <- genAE n
              t <- genAE n
              return (Minus s t)

genMult :: Int -> Gen AE
genMult n = do
              s <- genAE n
              t <- genAE n
              return (Mult s t)

genDiv :: Int -> Gen AE
genDiv n = do
              s <- genAE n
              t <- genAE n
              return (Div s t)

genIf0 :: Int -> Gen AE
genIf0 n = do
              s <- genAE n
              t <- genAE n
              u <- genAE n
              return (If0 s t u)

genAE :: Int -> Gen AE
genAE 0 = do
            term <- genNum
            return term
genAE n = do
            term <- oneof [genNum, (genPlus (n-1)), (genMinus (n-1)), (genMult (n-1)), (genDiv (n-1)), (genIf0 (n-1))]
            return term

testParser :: Int -> IO()
testParser n = quickCheckWith stdArgs {maxSuccess = n}
              (\t -> parseAE (pprint t) == t)

--It will stop a few runs because of bad input, have no idea how to avoid bad input
--Assuming its because the error message is not matching, we made error message whatevet we want
--But for EvalAEMaybe and EvalM error is simply Nothing and Nothing doesn't stop program and wont cause error to crash from my observation
testEvalAE :: Int -> IO()
testEvalAE n = quickCheckWith stdArgs {maxSuccess = n}
              (\t -> (interpAE (pprint t)) ==  Just(evalAE t))

testEvalAEMaybe :: Int -> IO()
testEvalAEMaybe n = quickCheckWith stdArgs {maxSuccess = n}
              (\t -> (interpAE (pprint t)) == (evalAEMaybe t))

testEvalM :: Int -> IO()
testEvalM n = quickCheckWith stdArgs {maxSuccess = n}
              (\t -> (interpAE (pprint t)) == (evalM t))
