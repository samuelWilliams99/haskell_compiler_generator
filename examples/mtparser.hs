module MtParser (runParser, module ParserRequirements, ASTCommand (..), ASTExpr (..), ASTDecl (..) , IncludeMap (..), IncludeMapType (..)) where

import ParserRequirements
import Control.Applicative
data ASTCommand = ASTAssign String ASTExpr ParseState
                | ASTCall String [ASTExpr] ParseState
                | ASTIf ASTExpr ASTCommand ASTCommand ParseState
                | ASTWhile ASTExpr ASTCommand ParseState
                | ASTLet [ASTDecl] ASTCommand ParseState
                | ASTSeq [ASTCommand] ParseState
                | ASTPass ParseState
data ASTExpr = ASTExprBinOp String ASTExpr ASTExpr ParseState
             | ASTExprInt Int ParseState
             | ASTExprBool Bool ParseState
             | ASTExprVar String ParseState
             | ASTExprUnOp String ASTExpr ParseState
data ASTDecl = ASTDeclConst String ASTExpr ParseState
             | ASTDeclVar String (Maybe ASTExpr) ParseState
unwrapOp :: TokenType -> String
unwrapOp (TokenOperator s) = s

gScanner = Scanner{ separateCasedIdentifiers=False
                  , ignoreWhitespace=True
                  , ignoreComments=True
                  , operators=["^","*","/","+","-","<","<=","==","!=",">=",">","&&","||","!",",",";",":",":=","="]
                  , keywords=["begin","const","do","else","end","if","in","let","then","var","while","true","false","pass","include"]
                  , lineComment=Just "//"
                  , blockComment=Nothing
                  , includeEOF=True
                  , parserMap=id
                  }

runParser str = do
    ts <- scan gScanner str
    let ps = if length ts == 0 then parseState "" else let (Token ps' _) = head ts in ps'
    generatedState0 ps [] [] $ fmap AbsSynToken ts

generatedError n [] = Error "Ran out of tokens"
generatedError n ((AbsSynToken (Token ps x)):xs) = Error $ "Unexpected token: " ++ (show x) ++ " at " ++ showPos ps

unpackFinal (AbsSynResult1 x _) = x

generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14, generatedState15, generatedState16, generatedState17, generatedState18, generatedState19, generatedState20, generatedState21, generatedState22, generatedState23, generatedState24, generatedState25, generatedState26, generatedState27, generatedState28, generatedState29, generatedState30, generatedState31, generatedState32, generatedState33, generatedState34, generatedState35, generatedState36, generatedState37, generatedState38, generatedState39, generatedState40, generatedState41, generatedState42, generatedState43, generatedState44, generatedState45, generatedState46, generatedState47, generatedState48, generatedState49, generatedState50, generatedState51, generatedState52, generatedState53, generatedState54, generatedState55, generatedState56, generatedState57, generatedState58, generatedState59, generatedState60, generatedState61, generatedState62, generatedState63, generatedState64, generatedState65, generatedState66, generatedState67, generatedState68, generatedState69, generatedState70, generatedState71, generatedState72, generatedState73, generatedState74, generatedState75, generatedState76, generatedState77, generatedState78, generatedState79, generatedState80, generatedState81, generatedState82, generatedState83, generatedState84]

data AbsSynToken t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 = AbsSynToken Token | AbsSynResult1 t1 ParseState | AbsSynResult2 t2 ParseState | AbsSynResult3 t3 ParseState | AbsSynResult4 t4 ParseState | AbsSynResult5 t5 ParseState | AbsSynResult6 t6 ParseState | AbsSynResult7 t7 ParseState | AbsSynResult8 t8 ParseState | AbsSynResult9 t9 ParseState | AbsSynResult10 t10 ParseState | AbsSynResult11 t11 ParseState | AbsSynResult12 t12 ParseState | AbsSynResult13 t13 ParseState | AbsSynResult14 t14 ParseState | AbsSynResult15 t15 ParseState

generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "include"))):xs) = generatedState1 ps (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState3 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult2 _ _):xs) = generatedState5 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult3 _ _):xs) = generatedState82 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult1 _ _):xs) = generatedState84 ps0 (x:vs) (0:ss) xs
generatedState0 _ _ _ xs = generatedError 0 xs


generatedState1 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState2 ps (x:vs) (1:ss) xs
generatedState1 _ _ _ xs = generatedError 1 xs


generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "include"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState2 _ _ _ xs = generatedError 2 xs


generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "include"))):xs) = generatedState1 ps (x:vs) (3:ss) xs
generatedState3 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState3 ps0 (x:vs) (3:ss) xs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynResult3 _ _):xs) = generatedState4 ps0 (x:vs) (3:ss) xs
generatedState3 _ _ _ xs = generatedError 3 xs


generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState4 _ _ _ xs = generatedError 4 xs


generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (5:ss) xs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (5:ss) xs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (5:ss) xs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (5:ss) xs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (5:ss) xs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (5:ss) xs
generatedState5 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState81 ps0 (x:vs) (5:ss) xs
generatedState5 _ _ _ xs = generatedError 5 xs


generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState6 _ _ _ xs = generatedError 6 xs


generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState76 ps0 (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState79 ps0 (x:vs) (7:ss) xs
generatedState7 _ _ _ xs = generatedError 7 xs


generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "var"))):xs) = generatedState9 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "const"))):xs) = generatedState47 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState51 ps0 (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState54 ps0 (x:vs) (8:ss) xs
generatedState8 _ _ _ xs = generatedError 8 xs


generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState10 ps (x:vs) (9:ss) xs
generatedState9 _ _ _ xs = generatedError 9 xs


generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ":="))):xs) = generatedState11 ps (x:vs) (10:ss) xs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState10 _ _ _ xs = generatedError 10 xs


generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState46 ps0 (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (11:ss) xs
generatedState11 _ _ _ xs = generatedError 11 xs


generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState12 _ _ _ xs = generatedError 12 xs


generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState13 _ _ _ xs = generatedError 13 xs


generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState21 ps0 (x:vs) (14:ss) xs
generatedState14 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (14:ss) xs
generatedState14 _ _ _ xs = generatedError 14 xs


generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (15:ss) xs
generatedState15 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState20 ps0 (x:vs) (15:ss) xs
generatedState15 _ _ _ xs = generatedError 15 xs


generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState16 _ _ _ xs = generatedError 16 xs


generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState17 _ _ _ xs = generatedError 17 xs


generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState18 _ _ _ xs = generatedError 18 xs


generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState19 _ _ _ xs = generatedError 19 xs


generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState20 _ _ _ xs = generatedError 20 xs


generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState32 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (21:ss) xs
generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = generatedState45 ps (x:vs) (21:ss) xs
generatedState21 _ _ _ xs = generatedError 21 xs


generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState22 _ _ _ xs = generatedError 22 xs


generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState23 _ _ _ xs = generatedError 23 xs


generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState24 _ _ _ xs = generatedError 24 xs


generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState25 _ _ _ xs = generatedError 25 xs


generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState26 _ _ _ xs = generatedError 26 xs


generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState27 _ _ _ xs = generatedError 27 xs


generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState28 _ _ _ xs = generatedError 28 xs


generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState29 _ _ _ xs = generatedError 29 xs


generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState30 _ _ _ xs = generatedError 30 xs


generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState31 _ _ _ xs = generatedError 31 xs


generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState33 ps0 (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (32:ss) xs
generatedState32 _ _ _ xs = generatedError 32 xs


generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState33 _ _ _ xs = generatedError 33 xs


generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState35 ps0 (x:vs) (34:ss) xs
generatedState34 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (34:ss) xs
generatedState34 _ _ _ xs = generatedError 34 xs


generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (35:ss) xs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState35 _ _ _ xs = generatedError 35 xs


generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState37 ps0 (x:vs) (36:ss) xs
generatedState36 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (36:ss) xs
generatedState36 _ _ _ xs = generatedError 36 xs


generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState37 _ _ _ xs = generatedError 37 xs


generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState39 ps0 (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (38:ss) xs
generatedState38 _ _ _ xs = generatedError 38 xs


generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (39:ss) xs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (39:ss) xs
generatedState39 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (39:ss) xs
generatedState39 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (39:ss) xs
generatedState39 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (39:ss) xs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (39:ss) xs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState39 _ _ _ xs = generatedError 39 xs


generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState41 ps0 (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (40:ss) xs
generatedState40 _ _ _ xs = generatedError 40 xs


generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState41 _ _ _ xs = generatedError 41 xs


generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState43 ps0 (x:vs) (42:ss) xs
generatedState42 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (42:ss) xs
generatedState42 _ _ _ xs = generatedError 42 xs


generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (43:ss) xs
generatedState43 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (43:ss) xs
generatedState43 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (43:ss) xs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (43:ss) xs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState43 _ _ _ xs = generatedError 43 xs


generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState44 _ _ _ xs = generatedError 44 xs


generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState45 _ _ _ xs = generatedError 45 xs


generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState32 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState46 _ _ _ xs = generatedError 46 xs


generatedState47 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState48 ps (x:vs) (47:ss) xs
generatedState47 _ _ _ xs = generatedError 47 xs


generatedState48 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "="))):xs) = generatedState49 ps (x:vs) (48:ss) xs
generatedState48 _ _ _ xs = generatedError 48 xs


generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState50 ps0 (x:vs) (49:ss) xs
generatedState49 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (49:ss) xs
generatedState49 _ _ _ xs = generatedError 49 xs


generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState32 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState50 _ _ _ xs = generatedError 50 xs


generatedState51 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState51 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = generatedState52 ps (x:vs) (51:ss) xs
generatedState51 _ _ _ xs = generatedError 51 xs


generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "var"))):xs) = generatedState9 ps (x:vs) (52:ss) xs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "const"))):xs) = generatedState47 ps (x:vs) (52:ss) xs
generatedState52 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState51 ps0 (x:vs) (52:ss) xs
generatedState52 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState53 ps0 (x:vs) (52:ss) xs
generatedState52 _ _ _ xs = generatedError 52 xs


generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState53 _ _ _ xs = generatedError 53 xs


generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "in"))):xs) = generatedState55 ps (x:vs) (54:ss) xs
generatedState54 _ _ _ xs = generatedError 54 xs


generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (55:ss) xs
generatedState55 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState75 ps0 (x:vs) (55:ss) xs
generatedState55 _ _ _ xs = generatedError 55 xs


generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState57 ps0 (x:vs) (56:ss) xs
generatedState56 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (56:ss) xs
generatedState56 _ _ _ xs = generatedError 56 xs


generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState32 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (57:ss) xs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "do"))):xs) = generatedState58 ps (x:vs) (57:ss) xs
generatedState57 _ _ _ xs = generatedError 57 xs


generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (58:ss) xs
generatedState58 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState74 ps0 (x:vs) (58:ss) xs
generatedState58 _ _ _ xs = generatedError 58 xs


generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState60 ps0 (x:vs) (59:ss) xs
generatedState59 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (59:ss) xs
generatedState59 _ _ _ xs = generatedError 59 xs


generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState32 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "then"))):xs) = generatedState61 ps (x:vs) (60:ss) xs
generatedState60 _ _ _ xs = generatedError 60 xs


generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState71 ps0 (x:vs) (61:ss) xs
generatedState61 _ _ _ xs = generatedError 61 xs


generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState63 ps (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ":="))):xs) = generatedState69 ps (x:vs) (62:ss) xs
generatedState62 _ _ _ xs = generatedError 62 xs


generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState64 ps0 (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState67 ps0 (x:vs) (63:ss) xs
generatedState63 _ _ _ xs = generatedError 63 xs


generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState32 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (64:ss) xs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = generatedState65 ps (x:vs) (64:ss) xs
generatedState64 _ _ _ xs = generatedError 64 xs


generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState64 ps0 (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (65:ss) xs
generatedState65 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState66 ps0 (x:vs) (65:ss) xs
generatedState65 _ _ _ xs = generatedError 65 xs


generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState66 _ _ _ xs = generatedError 66 xs


generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = generatedState68 ps (x:vs) (67:ss) xs
generatedState67 _ _ _ xs = generatedError 67 xs


generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState68 _ _ _ xs = generatedError 68 xs


generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!"))):xs) = generatedState12 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState13 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState14 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState15 ps0 (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "false"))):xs) = generatedState16 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "true"))):xs) = generatedState17 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState19 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState70 ps0 (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState44 ps0 (x:vs) (69:ss) xs
generatedState69 _ _ _ xs = generatedError 69 xs


generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">"))):xs) = generatedState22 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ">="))):xs) = generatedState23 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "!="))):xs) = generatedState24 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=="))):xs) = generatedState25 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<="))):xs) = generatedState26 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "<"))):xs) = generatedState27 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "-"))):xs) = generatedState28 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "+"))):xs) = generatedState29 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "/"))):xs) = generatedState30 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*"))):xs) = generatedState31 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "||"))):xs) = generatedState32 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "&&"))):xs) = generatedState34 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState36 ps0 (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState38 ps0 (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState40 ps0 (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^"))):xs) = generatedState42 ps (x:vs) (70:ss) xs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState70 _ _ _ xs = generatedError 70 xs


generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = generatedState72 ps (x:vs) (71:ss) xs
generatedState71 _ _ _ xs = generatedError 71 xs


generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (72:ss) xs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (72:ss) xs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (72:ss) xs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (72:ss) xs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (72:ss) xs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (72:ss) xs
generatedState72 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState73 ps0 (x:vs) (72:ss) xs
generatedState72 _ _ _ xs = generatedError 72 xs


generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState73 _ _ _ xs = generatedError 73 xs


generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState74 _ _ _ xs = generatedError 74 xs


generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState75 _ _ _ xs = generatedError 75 xs


generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = generatedState77 ps (x:vs) (76:ss) xs
generatedState76 _ _ _ xs = generatedError 76 xs


generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (77:ss) xs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (77:ss) xs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (77:ss) xs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (77:ss) xs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (77:ss) xs
generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (77:ss) xs
generatedState77 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState76 ps0 (x:vs) (77:ss) xs
generatedState77 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState78 ps0 (x:vs) (77:ss) xs
generatedState77 _ _ _ xs = generatedError 77 xs


generatedState78 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState78 _ _ _ xs = generatedError 78 xs


generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = generatedState80 ps (x:vs) (79:ss) xs
generatedState79 _ _ _ xs = generatedError 79 xs


generatedState80 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "end"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState80 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ";"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState80 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "else"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState80 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState80 _ _ _ xs = generatedError 80 xs


generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction2 ps0 vs
generatedState81 _ _ _ xs = generatedError 81 xs


generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "pass"))):xs) = generatedState6 ps (x:vs) (82:ss) xs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "begin"))):xs) = generatedState7 ps (x:vs) (82:ss) xs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "let"))):xs) = generatedState8 ps (x:vs) (82:ss) xs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "while"))):xs) = generatedState56 ps (x:vs) (82:ss) xs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "if"))):xs) = generatedState59 ps (x:vs) (82:ss) xs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState62 ps (x:vs) (82:ss) xs
generatedState82 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState83 ps0 (x:vs) (82:ss) xs
generatedState82 _ _ _ xs = generatedError 82 xs


generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction1 ps0 vs
generatedState83 _ _ _ xs = generatedError 83 xs


generatedState84 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState84 _ _ _ xs = generatedError 84 xs


generatedReduction1 ps0 ((AbsSynResult5 v2 ps2):(AbsSynResult3 v1 ps1):_) = AbsSynResult1 ((v2, v1)) ps1

generatedReduction2 ps0 ((AbsSynResult5 v2 ps2):(AbsSynResult2 v1 ps1):_) = AbsSynResult1 ((v2, v1)) ps1

generatedReduction3 ps0 (_) = AbsSynResult2 (empty) ps0

generatedReduction4 ps0 ((AbsSynResult3 v2 ps2):(AbsSynResult4 v1 ps1):_) = AbsSynResult3 (v1:v2) ps1

generatedReduction5 ps0 ((AbsSynResult4 v1 ps1):_) = AbsSynResult3 ([v1]) ps1

generatedReduction6 ps0 ((AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult4 ((v2, everything)) ps1

generatedReduction7 ps0 ((AbsSynResult9 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult5 (ASTAssign v1 v3 ps1) ps1

generatedReduction8 ps0 ((AbsSynToken (Token ps4 v4)):(AbsSynResult6 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult5 (ASTCall v1 v3 ps1) ps1

generatedReduction9 ps0 ((AbsSynResult5 v6 ps6):(AbsSynToken (Token ps5 v5)):(AbsSynResult5 v4 ps4):(AbsSynToken (Token ps3 v3)):(AbsSynResult9 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult5 (ASTIf v2 v4 v6 ps1) ps1

generatedReduction10 ps0 ((AbsSynResult5 v4 ps4):(AbsSynToken (Token ps3 v3)):(AbsSynResult9 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult5 (ASTWhile v2 v4 ps1) ps1

generatedReduction11 ps0 ((AbsSynResult5 v4 ps4):(AbsSynToken (Token ps3 v3)):(AbsSynResult7 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult5 (ASTLet v2 v4 ps1) ps1

generatedReduction12 ps0 ((AbsSynToken (Token ps3 v3)):(AbsSynResult8 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult5 (ASTSeq v2 ps1) ps1

generatedReduction13 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult5 (ASTPass ps1) ps1

generatedReduction14 ps0 ((AbsSynResult6 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult9 v1 ps1):_) = AbsSynResult6 (v1:v3) ps1

generatedReduction15 ps0 ((AbsSynResult9 v1 ps1):_) = AbsSynResult6 ([v1]) ps1

generatedReduction16 ps0 ((AbsSynResult7 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult15 v1 ps1):_) = AbsSynResult7 (v1:v3) ps1

generatedReduction17 ps0 ((AbsSynResult15 v1 ps1):_) = AbsSynResult7 ([v1]) ps1

generatedReduction18 ps0 ((AbsSynResult8 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult5 v1 ps1):_) = AbsSynResult8 (v1:v3) ps1

generatedReduction19 ps0 ((AbsSynResult5 v1 ps1):_) = AbsSynResult8 ([v1]) ps1

generatedReduction20 ps0 ((AbsSynResult13 v1 ps1):_) = AbsSynResult9 (v1) ps1

generatedReduction21 ps0 ((AbsSynResult9 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult9 v1 ps1):_) = AbsSynResult9 (ASTExprBinOp "^" v1 v3 ps2) ps1

generatedReduction22 ps0 ((AbsSynResult9 v3 ps3):(AbsSynResult10 v2 ps2):(AbsSynResult9 v1 ps1):_) = AbsSynResult9 (ASTExprBinOp (unwrapOp v2) v1 v3 ps2) ps1

generatedReduction23 ps0 ((AbsSynResult9 v3 ps3):(AbsSynResult11 v2 ps2):(AbsSynResult9 v1 ps1):_) = AbsSynResult9 (ASTExprBinOp (unwrapOp v2) v1 v3 ps2) ps1

generatedReduction24 ps0 ((AbsSynResult9 v3 ps3):(AbsSynResult12 v2 ps2):(AbsSynResult9 v1 ps1):_) = AbsSynResult9 (ASTExprBinOp (unwrapOp v2) v1 v3 ps2) ps1

generatedReduction25 ps0 ((AbsSynResult9 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult9 v1 ps1):_) = AbsSynResult9 (ASTExprBinOp "&&" v1 v3 ps2) ps1

generatedReduction26 ps0 ((AbsSynResult9 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult9 v1 ps1):_) = AbsSynResult9 (ASTExprBinOp "||" v1 v3 ps2) ps1

generatedReduction27 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult10 (v1) ps1

generatedReduction28 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult10 (v1) ps1

generatedReduction29 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult11 (v1) ps1

generatedReduction30 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult11 (v1) ps1

generatedReduction31 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v1) ps1

generatedReduction32 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v1) ps1

generatedReduction33 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v1) ps1

generatedReduction34 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v1) ps1

generatedReduction35 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v1) ps1

generatedReduction36 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v1) ps1

generatedReduction37 ps0 ((AbsSynToken (Token ps1 (TokenIntLit v1))):_) = AbsSynResult13 (ASTExprInt v1 ps1) ps1

generatedReduction38 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult13 (ASTExprVar v1 ps1) ps1

generatedReduction39 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult13 (ASTExprBool True ps1) ps1

generatedReduction40 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult13 (ASTExprBool False ps1) ps1

generatedReduction41 ps0 ((AbsSynResult13 v2 ps2):(AbsSynResult14 v1 ps1):_) = AbsSynResult13 (ASTExprUnOp (unwrapOp v1) v2 ps1) ps1

generatedReduction42 ps0 ((AbsSynToken (Token ps3 v3)):(AbsSynResult9 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult13 (v2) ps1

generatedReduction43 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult14 (v1) ps1

generatedReduction44 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult14 (v1) ps1

generatedReduction45 ps0 ((AbsSynResult9 v4 ps4):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult15 (ASTDeclConst v2 v4 ps1) ps1

generatedReduction46 ps0 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult15 (ASTDeclVar v2 Nothing ps1) ps1

generatedReduction47 ps0 ((AbsSynResult9 v4 ps4):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult15 (ASTDeclVar v2 (Just v4) ps1) ps1



data IncludeMap = IncludeMap{ _includeMapType :: IncludeMapType
                            , _nextIncludeMap :: Maybe IncludeMap } deriving Show
data IncludeMapType = IncludeMapEverything | IncludeMapWhitelist [String] | IncludeMapBlacklist [String] | IncludeMapRename [(String, String)] deriving Show
infixr 0 `andThen`
andThen :: IncludeMap -> IncludeMap -> IncludeMap
andThen a b = a { _nextIncludeMap = Just b }
everything :: IncludeMap
everything = IncludeMap IncludeMapEverything Nothing
whitelist :: [String] -> IncludeMap
whitelist xs = IncludeMap (IncludeMapWhitelist xs) Nothing
blacklist :: [String] -> IncludeMap
blacklist xs = IncludeMap (IncludeMapBlacklist xs) Nothing
rename :: [(String, String)] -> IncludeMap
rename ns = IncludeMap (IncludeMapRename ns) Nothing
