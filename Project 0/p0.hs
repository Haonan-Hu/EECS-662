{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad()
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--for quickcheck
import Test.QuickCheck

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
evalAE (Num n) = let r = n in
                    if (r < 0) then error "*" else r
evalAE (Plus l r) = (evalAE l) + (evalAE r) 
evalAE (Minus l r) = let r' = (evalAE l) - (evalAE r) in
                      if(r' < 0) then error "!" else r'
evalAE (Mult l r) = (evalAE l) * (evalAE r)
evalAE (Div l r) = let r' = (evalAE r) in
                    if(r' == 0) then error "!!" else (evalAE l) `div` r'
evalAE (If0 c t e) = if((evalAE c) == 0) then (evalAE t) else (evalAE e)

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num n) = let r = n in
                        if (r < 0) then Nothing else Just r
evalAEMaybe (Plus l r) = Just (evalAE (Plus l r))
evalAEMaybe (Minus l r) = let x = evalAE (Minus l r) in
                            if x < 0 then Nothing else Just x
evalAEMaybe (Mult l r) = Just (evalAE (Mult l r))
evalAEMaybe (Div l r) = let r' = (evalAE r) in
                          if(r' == 0) then Nothing else Just (evalAE (Div l r))
evalAEMaybe (If0 c t e) = if((evalAE c) == 0) then Just (evalAE t) else Just (evalAE e)

evalM :: AE -> Maybe Int
evalM (Num n) = do 
                 x <- if (n < 0) then Nothing else Just n
                 return x
evalM (Plus l r) = do
                    x <- (evalAEMaybe l)
                    y <- (evalAEMaybe r)
                    z <-  Just (x + y)
                    return z
evalM (Minus l r) = do
                    x <- (evalAEMaybe l)
                    y <- (evalAEMaybe r)
                    z <- if (x - y) < 0 then Nothing else Just (x - y)
                    return (z)
evalM (Mult l r) = do
                    x <- (evalAEMaybe l)
                    y <- (evalAEMaybe r)
                    z <-  Just (x * y)
                    return z
evalM (Div l r) = do
                    x <- (evalAEMaybe l)
                    y <- (evalAEMaybe r)
                    z <- if y == 0 then Nothing else Just (x `div` y)
                    return (z)
evalM (If0 c t e) = do
                    x <- if((evalAE c) == 0) then (evalAEMaybe t) else (evalAEMaybe e)
                    return x

interpAE :: String -> Maybe Int
interpAE x = evalM (parseAE x)

--Testing stuff
instance Arbitrary AE where
  arbitrary = sized $ \n -> genAE (rem n 10)

genNum = do
          t <- choose (0,100)
          return (Num t)

genPlus n = do
              s <- genAE n
              t <- genAE n
              return (Plus s t)

genMinus n = do
              s <- genAE n
              t <- genAE n
              return (Minus s t)

genMult n = do
              s <- genAE n
              t <- genAE n
              return (Mult s t)

genDiv n = do
              s <- genAE n
              t <- genAE n
              return (Div s t)

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
testEvalAE :: Int -> IO()
testEvalAE n = quickCheckWith stdArgs {maxSuccess = n}
              (\t -> (interpAE (pprint t)) == (evalAEMaybe t))