module MtGeneratedParser (runParser, module ParserRequirements, ASTCommand (..), ASTExpr (..), ASTDecl (..)) where

import ParserRequirements
import Control.Applicative
data ASTCommand = ASTAssign String ASTExpr
                | ASTCall String [ASTExpr]
                | ASTIf ASTExpr ASTCommand ASTCommand
                | ASTWhile ASTExpr ASTCommand
                | ASTLet [ASTDecl] ASTCommand
                | ASTSeq [ASTCommand]
                | ASTPass
data ASTExpr = ASTExprBinOp String ASTExpr ASTExpr
             | ASTExprInt Int
             | ASTExprBool Bool
             | ASTExprVar String
             | ASTExprUnOp String ASTExpr
data ASTDecl = ASTDeclConst String ASTExpr
             | ASTDeclVar String (Maybe ASTExpr)
unwrapOp :: TokenType -> String
unwrapOp (TokenOperator s) = s

gScanner = Scanner{ separateCasedIdentifiers=False
                  , ignoreWhitespace=True
                  , ignoreComments=True
                  , operators=["^","*","/","+","-","<","<=","==","!=",">=",">","&&","||","!",",",";",":",":=","="]
                  , keywords=["begin","const","do","else","end","if","in","let","then","var","while","true","false","pass"]
                  , lineComment=Just "//"
                  , blockComment=Nothing
                  , includeEOF=True
                  , parserMap=id
                  }

runParser str = do
    ts <- scan gScanner str
    generatedState0 [] [] $ fmap AbsSynToken ts

generatedError n [] = Error "Ran out of tokens"
generatedError n ((AbsSynToken (Token ps x)):xs) = Error $ "Unexpected token: " ++ (show x) ++ " at " ++ showPos ps

unpackFinal (AbsSynResult1 x) = x

generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14, generatedState15, generatedState16, generatedState17, generatedState18, generatedState19, generatedState20, generatedState21, generatedState22, generatedState23, generatedState24, generatedState25, generatedState26, generatedState27, generatedState28, generatedState29, generatedState30, generatedState31, generatedState32, generatedState33, generatedState34, generatedState35, generatedState36, generatedState37, generatedState38, generatedState39, generatedState40, generatedState41, generatedState42, generatedState43, generatedState44, generatedState45, generatedState46, generatedState47, generatedState48, generatedState49, generatedState50, generatedState51, generatedState52, generatedState53, generatedState54, generatedState55, generatedState56, generatedState57, generatedState58, generatedState59, generatedState60, generatedState61, generatedState62, generatedState63, generatedState64, generatedState65, generatedState66, generatedState67, generatedState68, generatedState69, generatedState70, generatedState71, generatedState72, generatedState73, generatedState74, generatedState75, generatedState76]

data AbsSynToken t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = AbsSynToken Token | AbsSynResult1 t1 | AbsSynResult2 t2 | AbsSynResult3 t3 | AbsSynResult4 t4 | AbsSynResult5 t5 | AbsSynResult6 t6 | AbsSynResult7 t7 | AbsSynResult8 t8 | AbsSynResult9 t9 | AbsSynResult10 t10 | AbsSynResult11 t11

generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState1 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState2 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState3 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState51 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState54 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult1 _):xs) = generatedState76 (x:vs) (0:ss) xs
generatedState0 vs ss xs = generatedError 0 xs


generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState1 vs ss xs = generatedError 1 xs


generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState1 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState2 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState3 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState51 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState54 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynResult1 _):xs) = generatedState71 (x:vs) (2:ss) xs
generatedState2 vs ss (x@(AbsSynResult4 _):xs) = generatedState74 (x:vs) (2:ss) xs
generatedState2 vs ss xs = generatedError 2 xs


generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "var"))):xs) = generatedState4 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "const"))):xs) = generatedState42 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynResult11 _):xs) = generatedState46 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynResult3 _):xs) = generatedState49 (x:vs) (3:ss) xs
generatedState3 vs ss xs = generatedError 3 xs


generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState5 (x:vs) (4:ss) xs
generatedState4 vs ss xs = generatedError 4 xs


generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenOperator ":="))):xs) = generatedState6 (x:vs) (5:ss) xs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState5 vs ss xs = generatedError 5 xs


generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynResult5 _):xs) = generatedState41 (x:vs) (6:ss) xs
generatedState6 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (6:ss) xs
generatedState6 vs ss xs = generatedError 6 xs


generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState7 vs ss xs = generatedError 7 xs


generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState8 vs ss xs = generatedError 8 xs


generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynResult5 _):xs) = generatedState16 (x:vs) (9:ss) xs
generatedState9 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (9:ss) xs
generatedState9 vs ss xs = generatedError 9 xs


generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (10:ss) xs
generatedState10 vs ss (x@(AbsSynResult9 _):xs) = generatedState15 (x:vs) (10:ss) xs
generatedState10 vs ss xs = generatedError 10 xs


generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState11 vs ss xs = generatedError 11 xs


generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState12 vs ss xs = generatedError 12 xs


generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState13 vs ss xs = generatedError 13 xs


generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState14 vs ss xs = generatedError 14 xs


generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState15 vs ss xs = generatedError 15 xs


generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState27 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = generatedState40 (x:vs) (16:ss) xs
generatedState16 vs ss xs = generatedError 16 xs


generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState17 vs ss xs = generatedError 17 xs


generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState18 vs ss xs = generatedError 18 xs


generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState19 vs ss xs = generatedError 19 xs


generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState20 vs ss xs = generatedError 20 xs


generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState21 vs ss xs = generatedError 21 xs


generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState22 vs ss xs = generatedError 22 xs


generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState23 vs ss xs = generatedError 23 xs


generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState24 vs ss xs = generatedError 24 xs


generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState25 vs ss xs = generatedError 25 xs


generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss xs = generatedError 26 xs


generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynResult5 _):xs) = generatedState28 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (27:ss) xs
generatedState27 vs ss xs = generatedError 27 xs


generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState28 vs ss xs = generatedError 28 xs


generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynResult5 _):xs) = generatedState30 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (29:ss) xs
generatedState29 vs ss xs = generatedError 29 xs


generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (30:ss) xs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState30 vs ss xs = generatedError 30 xs


generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynResult5 _):xs) = generatedState32 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (31:ss) xs
generatedState31 vs ss xs = generatedError 31 xs


generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState32 vs ss xs = generatedError 32 xs


generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynResult5 _):xs) = generatedState34 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (33:ss) xs
generatedState33 vs ss xs = generatedError 33 xs


generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (34:ss) xs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState34 vs ss xs = generatedError 34 xs


generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynResult5 _):xs) = generatedState36 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (35:ss) xs
generatedState35 vs ss xs = generatedError 35 xs


generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState36 vs ss xs = generatedError 36 xs


generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynResult5 _):xs) = generatedState38 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (37:ss) xs
generatedState37 vs ss xs = generatedError 37 xs


generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState38 vs ss xs = generatedError 38 xs


generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState39 vs ss xs = generatedError 39 xs


generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss xs = generatedError 40 xs


generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState27 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState41 vs ss xs = generatedError 41 xs


generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState43 (x:vs) (42:ss) xs
generatedState42 vs ss xs = generatedError 42 xs


generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenOperator "="))):xs) = generatedState44 (x:vs) (43:ss) xs
generatedState43 vs ss xs = generatedError 43 xs


generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynResult5 _):xs) = generatedState45 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (44:ss) xs
generatedState44 vs ss xs = generatedError 44 xs


generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState27 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState45 vs ss xs = generatedError 45 xs


generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = generatedState47 (x:vs) (46:ss) xs
generatedState46 vs ss xs = generatedError 46 xs


generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "var"))):xs) = generatedState4 (x:vs) (47:ss) xs
generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "const"))):xs) = generatedState42 (x:vs) (47:ss) xs
generatedState47 vs ss (x@(AbsSynResult11 _):xs) = generatedState46 (x:vs) (47:ss) xs
generatedState47 vs ss (x@(AbsSynResult3 _):xs) = generatedState48 (x:vs) (47:ss) xs
generatedState47 vs ss xs = generatedError 47 xs


generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState48 vs ss xs = generatedError 48 xs


generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = generatedState50 (x:vs) (49:ss) xs
generatedState49 vs ss xs = generatedError 49 xs


generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState1 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState2 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState3 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState51 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState54 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (x:vs) (50:ss) xs
generatedState50 vs ss (x@(AbsSynResult1 _):xs) = generatedState70 (x:vs) (50:ss) xs
generatedState50 vs ss xs = generatedError 50 xs


generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynResult5 _):xs) = generatedState52 (x:vs) (51:ss) xs
generatedState51 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (51:ss) xs
generatedState51 vs ss xs = generatedError 51 xs


generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState27 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = generatedState53 (x:vs) (52:ss) xs
generatedState52 vs ss xs = generatedError 52 xs


generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState1 (x:vs) (53:ss) xs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState2 (x:vs) (53:ss) xs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState3 (x:vs) (53:ss) xs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState51 (x:vs) (53:ss) xs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState54 (x:vs) (53:ss) xs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (x:vs) (53:ss) xs
generatedState53 vs ss (x@(AbsSynResult1 _):xs) = generatedState69 (x:vs) (53:ss) xs
generatedState53 vs ss xs = generatedError 53 xs


generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynResult5 _):xs) = generatedState55 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (54:ss) xs
generatedState54 vs ss xs = generatedError 54 xs


generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState27 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = generatedState56 (x:vs) (55:ss) xs
generatedState55 vs ss xs = generatedError 55 xs


generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState1 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState2 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState3 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState51 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState54 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (x:vs) (56:ss) xs
generatedState56 vs ss (x@(AbsSynResult1 _):xs) = generatedState66 (x:vs) (56:ss) xs
generatedState56 vs ss xs = generatedError 56 xs


generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState58 (x:vs) (57:ss) xs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenOperator ":="))):xs) = generatedState64 (x:vs) (57:ss) xs
generatedState57 vs ss xs = generatedError 57 xs


generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynResult5 _):xs) = generatedState59 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (58:ss) xs
generatedState58 vs ss (x@(AbsSynResult2 _):xs) = generatedState62 (x:vs) (58:ss) xs
generatedState58 vs ss xs = generatedError 58 xs


generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState27 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = generatedState60 (x:vs) (59:ss) xs
generatedState59 vs ss xs = generatedError 59 xs


generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynResult5 _):xs) = generatedState59 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynResult2 _):xs) = generatedState61 (x:vs) (60:ss) xs
generatedState60 vs ss xs = generatedError 60 xs


generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState61 vs ss xs = generatedError 61 xs


generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = generatedState63 (x:vs) (62:ss) xs
generatedState62 vs ss xs = generatedError 62 xs


generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction2 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction2 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction2 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction2 vs
generatedState63 vs ss xs = generatedError 63 xs


generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState7 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState8 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState9 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynResult10 _):xs) = generatedState10 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState11 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState12 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState14 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynResult5 _):xs) = generatedState65 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynResult9 _):xs) = generatedState39 (x:vs) (64:ss) xs
generatedState64 vs ss xs = generatedError 64 xs


generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState17 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState18 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState19 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState20 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState21 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState22 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState23 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState24 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState25 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState26 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState27 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState29 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynResult8 _):xs) = generatedState31 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynResult7 _):xs) = generatedState33 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynResult6 _):xs) = generatedState35 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState37 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction1 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction1 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction1 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction1 vs
generatedState65 vs ss xs = generatedError 65 xs


generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = generatedState67 (x:vs) (66:ss) xs
generatedState66 vs ss xs = generatedError 66 xs


generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState1 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState2 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState3 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState51 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState54 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynResult1 _):xs) = generatedState68 (x:vs) (67:ss) xs
generatedState67 vs ss xs = generatedError 67 xs


generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction3 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction3 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction3 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction3 vs
generatedState68 vs ss xs = generatedError 68 xs


generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction4 vs
generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction4 vs
generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction4 vs
generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction4 vs
generatedState69 vs ss xs = generatedError 69 xs


generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState70 vs ss xs = generatedError 70 xs


generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = generatedState72 (x:vs) (71:ss) xs
generatedState71 vs ss xs = generatedError 71 xs


generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState1 (x:vs) (72:ss) xs
generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState2 (x:vs) (72:ss) xs
generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState3 (x:vs) (72:ss) xs
generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState51 (x:vs) (72:ss) xs
generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState54 (x:vs) (72:ss) xs
generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (x:vs) (72:ss) xs
generatedState72 vs ss (x@(AbsSynResult1 _):xs) = generatedState71 (x:vs) (72:ss) xs
generatedState72 vs ss (x@(AbsSynResult4 _):xs) = generatedState73 (x:vs) (72:ss) xs
generatedState72 vs ss xs = generatedError 72 xs


generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction12 vs
generatedState73 vs ss xs = generatedError 73 xs


generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = generatedState75 (x:vs) (74:ss) xs
generatedState74 vs ss xs = generatedError 74 xs


generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState75 vs ss xs = generatedError 75 xs


generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState76 vs ss xs = generatedError 76 xs


generatedReduction1 ((AbsSynResult5 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult1 (ASTAssign v1 v3)

generatedReduction2 ((AbsSynToken (Token ps4 v4)):(AbsSynResult2 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult1 (ASTCall v1 v3)

generatedReduction3 ((AbsSynResult1 v6):(AbsSynToken (Token ps5 v5)):(AbsSynResult1 v4):(AbsSynToken (Token ps3 v3)):(AbsSynResult5 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult1 (ASTIf v2 v4 v6)

generatedReduction4 ((AbsSynResult1 v4):(AbsSynToken (Token ps3 v3)):(AbsSynResult5 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult1 (ASTWhile v2 v4)

generatedReduction5 ((AbsSynResult1 v4):(AbsSynToken (Token ps3 v3)):(AbsSynResult3 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult1 (ASTLet v2 v4)

generatedReduction6 ((AbsSynToken (Token ps3 v3)):(AbsSynResult4 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult1 (ASTSeq v2)

generatedReduction7 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult1 (ASTPass)

generatedReduction8 ((AbsSynResult2 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult5 v1):_) = AbsSynResult2 (v1:v3)

generatedReduction9 ((AbsSynResult5 v1):_) = AbsSynResult2 ([v1])

generatedReduction10 ((AbsSynResult3 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult11 v1):_) = AbsSynResult3 (v1:v3)

generatedReduction11 ((AbsSynResult11 v1):_) = AbsSynResult3 ([v1])

generatedReduction12 ((AbsSynResult4 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult1 v1):_) = AbsSynResult4 (v1:v3)

generatedReduction13 ((AbsSynResult1 v1):_) = AbsSynResult4 ([v1])

generatedReduction14 ((AbsSynResult9 v1):_) = AbsSynResult5 (v1)

generatedReduction15 ((AbsSynResult5 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult5 v1):_) = AbsSynResult5 (ASTExprBinOp "^" v1 v3)

generatedReduction16 ((AbsSynResult5 v3):(AbsSynResult6 v2):(AbsSynResult5 v1):_) = AbsSynResult5 (ASTExprBinOp (unwrapOp v2) v1 v3)

generatedReduction17 ((AbsSynResult5 v3):(AbsSynResult7 v2):(AbsSynResult5 v1):_) = AbsSynResult5 (ASTExprBinOp (unwrapOp v2) v1 v3)

generatedReduction18 ((AbsSynResult5 v3):(AbsSynResult8 v2):(AbsSynResult5 v1):_) = AbsSynResult5 (ASTExprBinOp (unwrapOp v2) v1 v3)

generatedReduction19 ((AbsSynResult5 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult5 v1):_) = AbsSynResult5 (ASTExprBinOp "&&" v1 v3)

generatedReduction20 ((AbsSynResult5 v3):(AbsSynToken (Token ps2 v2)):(AbsSynResult5 v1):_) = AbsSynResult5 (ASTExprBinOp "||" v1 v3)

generatedReduction21 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult6 (v1)

generatedReduction22 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult6 (v1)

generatedReduction23 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult7 (v1)

generatedReduction24 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult7 (v1)

generatedReduction25 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v1)

generatedReduction26 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v1)

generatedReduction27 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v1)

generatedReduction28 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v1)

generatedReduction29 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v1)

generatedReduction30 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v1)

generatedReduction31 ((AbsSynToken (Token ps1 (TokenIntLit v1))):_) = AbsSynResult9 (ASTExprInt v1)

generatedReduction32 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult9 (ASTExprVar v1)

generatedReduction33 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult9 (ASTExprBool True)

generatedReduction34 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult9 (ASTExprBool False)

generatedReduction35 ((AbsSynResult9 v2):(AbsSynResult10 v1):_) = AbsSynResult9 (ASTExprUnOp (unwrapOp v1) v2)

generatedReduction36 ((AbsSynToken (Token ps3 v3)):(AbsSynResult5 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult9 (v2)

generatedReduction37 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult10 (v1)

generatedReduction38 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult10 (v1)

generatedReduction39 ((AbsSynResult5 v4):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult11 (ASTDeclConst v2 v4)

generatedReduction40 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult11 (ASTDeclVar v2 Nothing)

generatedReduction41 ((AbsSynResult5 v4):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult11 (ASTDeclVar v2 $ Just v4)

