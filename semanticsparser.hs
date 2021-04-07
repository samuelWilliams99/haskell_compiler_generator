module Semanticsparser (runParser, module ParserRequirements) where

import ParserRequirements
import Control.Applicative
import Data.Maybe
import Data.Either
import Data.List
import Semantics
import Control.Arrow
handleStatements :: [Either SemanticsRule String] -> ([SemanticsRule], [String])
handleStatements stmts = aux stmts ""
  where
    aux [] _ = ([], [])
    aux ((Left rule):stmts) t = first (rule{ _semanticsRuleAstType=t }:) $ aux stmts t
    aux ((Right t):stmts) _ = second (union [t]) $ aux stmts t

gScanner = Scanner{ separateCasedIdentifiers=True
                  , ignoreWhitespace=True
                  , ignoreComments=True
                  , operators=["->","=>","<-","@",",","*->","*=>","^->","^=>"]
                  , keywords=["evaluating","where","restricting","to","case"]
                  , lineComment=Just "#"
                  , blockComment=Just ("#[","]#")
                  , includeEOF=True
                  , parserMap=(languageDefsParser:)
                  }

runParser str = do
    ts <- scan gScanner str
    generatedState0 [] [] $ fmap AbsSynToken ts

generatedError n [] = Error "Ran out of tokens"
generatedError n ((AbsSynToken (Token ps x)):xs) = Error $ "Unexpected token: " ++ (show x) ++ " at " ++ showPos ps

unpackFinal (AbsSynResult1 x) = x

generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14, generatedState15, generatedState16, generatedState17, generatedState18, generatedState19, generatedState20, generatedState21, generatedState22, generatedState23, generatedState24, generatedState25, generatedState26, generatedState27, generatedState28, generatedState29, generatedState30, generatedState31, generatedState32, generatedState33, generatedState34, generatedState35, generatedState36, generatedState37, generatedState38, generatedState39, generatedState40, generatedState41, generatedState42, generatedState43, generatedState44, generatedState45, generatedState46, generatedState47, generatedState48, generatedState49, generatedState50, generatedState51, generatedState52, generatedState53, generatedState54, generatedState55, generatedState56, generatedState57, generatedState58, generatedState59, generatedState60, generatedState61, generatedState62, generatedState63, generatedState64, generatedState65, generatedState66, generatedState67, generatedState68, generatedState69, generatedState70, generatedState71, generatedState72, generatedState73, generatedState74, generatedState75, generatedState76, generatedState77, generatedState78, generatedState79, generatedState80, generatedState81, generatedState82, generatedState83, generatedState84, generatedState85, generatedState86, generatedState87, generatedState88, generatedState89, generatedState90, generatedState91, generatedState92, generatedState93, generatedState94, generatedState95, generatedState96, generatedState97, generatedState98, generatedState99, generatedState100, generatedState101, generatedState102]

data AbsSynToken t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 = AbsSynToken Token | AbsSynResult1 t1 | AbsSynResult2 t2 | AbsSynResult3 t3 | AbsSynResult4 t4 | AbsSynResult5 t5 | AbsSynResult6 t6 | AbsSynResult7 t7 | AbsSynResult8 t8 | AbsSynResult9 t9 | AbsSynResult10 t10 | AbsSynResult11 t11 | AbsSynResult12 t12 | AbsSynResult13 t13 | AbsSynResult14 t14 | AbsSynResult15 t15 | AbsSynResult16 t16 | AbsSynResult17 t17 | AbsSynResult18 t18 | AbsSynResult19 t19 | AbsSynResult20 t20 | AbsSynResult21 t21 | AbsSynResult22 t22 | AbsSynResult23 t23 | AbsSynResult24 t24 | AbsSynResult25 t25 | AbsSynResult26 t26 | AbsSynResult27 t27 | AbsSynResult28 t28

generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "extension"))):xs) = generatedState1 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult6 _):xs) = generatedState3 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult1 _):xs) = generatedState102 (x:vs) (0:ss) xs
generatedState0 vs ss xs = generatedError 0 xs


generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState2 (x:vs) (1:ss) xs
generatedState1 vs ss xs = generatedError 1 xs


generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState2 vs ss xs = generatedError 2 xs


generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = generatedState4 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState3 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = generatedState3 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState3 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState3 vs ss (x@(AbsSynResult7 _):xs) = generatedState6 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynResult2 _):xs) = generatedState7 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynResult3 _):xs) = generatedState93 (x:vs) (3:ss) xs
generatedState3 vs ss xs = generatedError 3 xs


generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState5 (x:vs) (4:ss) xs
generatedState4 vs ss xs = generatedError 4 xs


generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState5 vs ss xs = generatedError 5 xs


generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState6 vs ss xs = generatedError 6 xs


generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState8 (x:vs) (7:ss) xs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = generatedState7 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState7 vs ss (x@(AbsSynResult8 _):xs) = generatedState11 (x:vs) (7:ss) xs
generatedState7 vs ss (x@(AbsSynResult4 _):xs) = generatedState12 (x:vs) (7:ss) xs
generatedState7 vs ss (x@(AbsSynResult5 _):xs) = generatedState89 (x:vs) (7:ss) xs
generatedState7 vs ss xs = generatedError 7 xs


generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState9 (x:vs) (8:ss) xs
generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState10 (x:vs) (8:ss) xs
generatedState8 vs ss xs = generatedError 8 xs


generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction12 vs
generatedState9 vs ss xs = generatedError 9 xs


generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState10 vs ss xs = generatedError 10 xs


generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState11 vs ss xs = generatedError 11 xs


generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = generatedState13 (x:vs) (12:ss) xs
generatedState12 vs ss (x@(AbsSynResult10 _):xs) = generatedState17 (x:vs) (12:ss) xs
generatedState12 vs ss xs = generatedError 12 xs


generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState14 (x:vs) (13:ss) xs
generatedState13 vs ss (x@(AbsSynResult11 _):xs) = generatedState16 (x:vs) (13:ss) xs
generatedState13 vs ss xs = generatedError 13 xs


generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState14 (x:vs) (14:ss) xs
generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "state"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState14 vs ss (x@(AbsSynResult11 _):xs) = generatedState15 (x:vs) (14:ss) xs
generatedState14 vs ss xs = generatedError 14 xs


generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "state"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState15 vs ss xs = generatedError 15 xs


generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "state"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState16 vs ss xs = generatedError 16 xs


generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "state"))):xs) = generatedState18 (x:vs) (17:ss) xs
generatedState17 vs ss (x@(AbsSynResult12 _):xs) = generatedState24 (x:vs) (17:ss) xs
generatedState17 vs ss xs = generatedError 17 xs


generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState19 (x:vs) (18:ss) xs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState20 (x:vs) (18:ss) xs
generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState21 (x:vs) (18:ss) xs
generatedState18 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (18:ss) xs
generatedState18 vs ss xs = generatedError 18 xs


generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState19 vs ss xs = generatedError 19 xs


generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState20 vs ss xs = generatedError 20 xs


generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState21 vs ss xs = generatedError 21 xs


generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState19 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState20 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState21 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynResult9 _):xs) = generatedState23 (x:vs) (22:ss) xs
generatedState22 vs ss xs = generatedError 22 xs


generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState23 vs ss xs = generatedError 23 xs


generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState25 (x:vs) (24:ss) xs
generatedState24 vs ss (x@(AbsSynResult16 _):xs) = generatedState27 (x:vs) (24:ss) xs
generatedState24 vs ss (x@(AbsSynResult13 _):xs) = generatedState88 (x:vs) (24:ss) xs
generatedState24 vs ss xs = generatedError 24 xs


generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState26 (x:vs) (25:ss) xs
generatedState25 vs ss xs = generatedError 25 xs


generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState26 vs ss xs = generatedError 26 xs


generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState25 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState28 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynResult16 _):xs) = generatedState83 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynResult17 _):xs) = generatedState84 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynResult15 _):xs) = generatedState85 (x:vs) (27:ss) xs
generatedState27 vs ss (x@(AbsSynResult14 _):xs) = generatedState87 (x:vs) (27:ss) xs
generatedState27 vs ss xs = generatedError 27 xs


generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState29 (x:vs) (28:ss) xs
generatedState28 vs ss xs = generatedError 28 xs


generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState30 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState77 (x:vs) (29:ss) xs
generatedState29 vs ss xs = generatedError 29 xs


generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState31 (x:vs) (30:ss) xs
generatedState30 vs ss xs = generatedError 30 xs


generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState31 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (31:ss) xs
generatedState31 vs ss (x@(AbsSynResult20 _):xs) = generatedState35 (x:vs) (31:ss) xs
generatedState31 vs ss xs = generatedError 31 xs


generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState33 (x:vs) (32:ss) xs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState34 (x:vs) (32:ss) xs
generatedState32 vs ss xs = generatedError 32 xs


generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState33 vs ss xs = generatedError 33 xs


generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss xs = generatedError 34 xs


generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState35 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState35 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState35 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState35 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState36 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynResult21 _):xs) = generatedState71 (x:vs) (35:ss) xs
generatedState35 vs ss xs = generatedError 35 xs


generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState37 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState47 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynResult23 _):xs) = generatedState57 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynResult22 _):xs) = generatedState59 (x:vs) (36:ss) xs
generatedState36 vs ss xs = generatedError 36 xs


generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^=>"))):xs) = generatedState38 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*=>"))):xs) = generatedState41 (x:vs) (37:ss) xs
generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState44 (x:vs) (37:ss) xs
generatedState37 vs ss xs = generatedError 37 xs


generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState39 (x:vs) (38:ss) xs
generatedState38 vs ss xs = generatedError 38 xs


generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState39 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (39:ss) xs
generatedState39 vs ss (x@(AbsSynResult20 _):xs) = generatedState40 (x:vs) (39:ss) xs
generatedState39 vs ss xs = generatedError 39 xs


generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState40 vs ss xs = generatedError 40 xs


generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState42 (x:vs) (41:ss) xs
generatedState41 vs ss xs = generatedError 41 xs


generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState42 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (42:ss) xs
generatedState42 vs ss (x@(AbsSynResult20 _):xs) = generatedState43 (x:vs) (42:ss) xs
generatedState42 vs ss xs = generatedError 42 xs


generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState43 vs ss xs = generatedError 43 xs


generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState45 (x:vs) (44:ss) xs
generatedState44 vs ss xs = generatedError 44 xs


generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState45 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (45:ss) xs
generatedState45 vs ss (x@(AbsSynResult20 _):xs) = generatedState46 (x:vs) (45:ss) xs
generatedState45 vs ss xs = generatedError 45 xs


generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState46 vs ss xs = generatedError 46 xs


generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = generatedState48 (x:vs) (47:ss) xs
generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = generatedState51 (x:vs) (47:ss) xs
generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState54 (x:vs) (47:ss) xs
generatedState47 vs ss xs = generatedError 47 xs


generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState49 (x:vs) (48:ss) xs
generatedState48 vs ss xs = generatedError 48 xs


generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState49 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (49:ss) xs
generatedState49 vs ss (x@(AbsSynResult20 _):xs) = generatedState50 (x:vs) (49:ss) xs
generatedState49 vs ss xs = generatedError 49 xs


generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState50 vs ss xs = generatedError 50 xs


generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState52 (x:vs) (51:ss) xs
generatedState51 vs ss xs = generatedError 51 xs


generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState52 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (52:ss) xs
generatedState52 vs ss (x@(AbsSynResult20 _):xs) = generatedState53 (x:vs) (52:ss) xs
generatedState52 vs ss xs = generatedError 52 xs


generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState53 vs ss xs = generatedError 53 xs


generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState55 (x:vs) (54:ss) xs
generatedState54 vs ss xs = generatedError 54 xs


generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState55 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynResult20 _):xs) = generatedState56 (x:vs) (55:ss) xs
generatedState55 vs ss xs = generatedError 55 xs


generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState56 vs ss xs = generatedError 56 xs


generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState37 (x:vs) (57:ss) xs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState47 (x:vs) (57:ss) xs
generatedState57 vs ss (x@(AbsSynResult23 _):xs) = generatedState57 (x:vs) (57:ss) xs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState57 vs ss (x@(AbsSynResult22 _):xs) = generatedState58 (x:vs) (57:ss) xs
generatedState57 vs ss xs = generatedError 57 xs


generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState58 vs ss xs = generatedError 58 xs


generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState59 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState59 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState59 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState59 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState60 (x:vs) (59:ss) xs
generatedState59 vs ss (x@(AbsSynResult24 _):xs) = generatedState70 (x:vs) (59:ss) xs
generatedState59 vs ss xs = generatedError 59 xs


generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState61 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynResult26 _):xs) = generatedState67 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynResult25 _):xs) = generatedState69 (x:vs) (60:ss) xs
generatedState60 vs ss xs = generatedError 60 xs


generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "to"))):xs) = generatedState62 (x:vs) (61:ss) xs
generatedState61 vs ss xs = generatedError 61 xs


generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState63 (x:vs) (62:ss) xs
generatedState62 vs ss (x@(AbsSynResult27 _):xs) = generatedState66 (x:vs) (62:ss) xs
generatedState62 vs ss xs = generatedError 62 xs


generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = generatedState64 (x:vs) (63:ss) xs
generatedState63 vs ss xs = generatedError 63 xs


generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState63 (x:vs) (64:ss) xs
generatedState64 vs ss (x@(AbsSynResult27 _):xs) = generatedState65 (x:vs) (64:ss) xs
generatedState64 vs ss xs = generatedError 64 xs


generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState65 vs ss xs = generatedError 65 xs


generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState66 vs ss xs = generatedError 66 xs


generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState61 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynResult26 _):xs) = generatedState67 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState67 vs ss (x@(AbsSynResult25 _):xs) = generatedState68 (x:vs) (67:ss) xs
generatedState67 vs ss xs = generatedError 67 xs


generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState68 vs ss xs = generatedError 68 xs


generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState69 vs ss xs = generatedError 69 xs


generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState70 vs ss xs = generatedError 70 xs


generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState72 (x:vs) (71:ss) xs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState71 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState71 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState71 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState71 vs ss (x@(AbsSynResult28 _):xs) = generatedState74 (x:vs) (71:ss) xs
generatedState71 vs ss (x@(AbsSynResult18 _):xs) = generatedState75 (x:vs) (71:ss) xs
generatedState71 vs ss (x@(AbsSynResult19 _):xs) = generatedState76 (x:vs) (71:ss) xs
generatedState71 vs ss xs = generatedError 71 xs


generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState73 (x:vs) (72:ss) xs
generatedState72 vs ss xs = generatedError 72 xs


generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss xs = generatedError 73 xs


generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState74 vs ss xs = generatedError 74 xs


generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState75 vs ss xs = generatedError 75 xs


generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState76 vs ss xs = generatedError 76 xs


generatedState77 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState78 (x:vs) (77:ss) xs
generatedState77 vs ss xs = generatedError 77 xs


generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState78 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState32 (x:vs) (78:ss) xs
generatedState78 vs ss (x@(AbsSynResult20 _):xs) = generatedState79 (x:vs) (78:ss) xs
generatedState78 vs ss xs = generatedError 78 xs


generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState79 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState79 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState79 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState79 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState36 (x:vs) (79:ss) xs
generatedState79 vs ss (x@(AbsSynResult21 _):xs) = generatedState80 (x:vs) (79:ss) xs
generatedState79 vs ss xs = generatedError 79 xs


generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState72 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState80 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState80 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState80 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState80 vs ss (x@(AbsSynResult28 _):xs) = generatedState74 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult18 _):xs) = generatedState81 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult19 _):xs) = generatedState82 (x:vs) (80:ss) xs
generatedState80 vs ss xs = generatedError 80 xs


generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState81 vs ss xs = generatedError 81 xs


generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState82 vs ss xs = generatedError 82 xs


generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState83 vs ss xs = generatedError 83 xs


generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState84 vs ss xs = generatedError 84 xs


generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState25 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState28 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynResult16 _):xs) = generatedState83 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynResult17 _):xs) = generatedState84 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynResult15 _):xs) = generatedState85 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState85 vs ss (x@(AbsSynResult14 _):xs) = generatedState86 (x:vs) (85:ss) xs
generatedState85 vs ss xs = generatedError 85 xs


generatedState86 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState86 vs ss xs = generatedError 86 xs


generatedState87 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState87 vs ss xs = generatedError 87 xs


generatedState88 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction4 vs
generatedState88 vs ss xs = generatedError 88 xs


generatedState89 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = generatedState13 (x:vs) (89:ss) xs
generatedState89 vs ss (x@(AbsSynResult10 _):xs) = generatedState90 (x:vs) (89:ss) xs
generatedState89 vs ss xs = generatedError 89 xs


generatedState90 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "state"))):xs) = generatedState18 (x:vs) (90:ss) xs
generatedState90 vs ss (x@(AbsSynResult12 _):xs) = generatedState91 (x:vs) (90:ss) xs
generatedState90 vs ss xs = generatedError 90 xs


generatedState91 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState25 (x:vs) (91:ss) xs
generatedState91 vs ss (x@(AbsSynResult16 _):xs) = generatedState27 (x:vs) (91:ss) xs
generatedState91 vs ss (x@(AbsSynResult13 _):xs) = generatedState92 (x:vs) (91:ss) xs
generatedState91 vs ss xs = generatedError 91 xs


generatedState92 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction3 vs
generatedState92 vs ss xs = generatedError 92 xs


generatedState93 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState8 (x:vs) (93:ss) xs
generatedState93 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = generatedState93 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState93 vs ss (x@(AbsSynResult8 _):xs) = generatedState11 (x:vs) (93:ss) xs
generatedState93 vs ss (x@(AbsSynResult4 _):xs) = generatedState94 (x:vs) (93:ss) xs
generatedState93 vs ss (x@(AbsSynResult5 _):xs) = generatedState98 (x:vs) (93:ss) xs
generatedState93 vs ss xs = generatedError 93 xs


generatedState94 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = generatedState13 (x:vs) (94:ss) xs
generatedState94 vs ss (x@(AbsSynResult10 _):xs) = generatedState95 (x:vs) (94:ss) xs
generatedState94 vs ss xs = generatedError 94 xs


generatedState95 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "state"))):xs) = generatedState18 (x:vs) (95:ss) xs
generatedState95 vs ss (x@(AbsSynResult12 _):xs) = generatedState96 (x:vs) (95:ss) xs
generatedState95 vs ss xs = generatedError 95 xs


generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState25 (x:vs) (96:ss) xs
generatedState96 vs ss (x@(AbsSynResult16 _):xs) = generatedState27 (x:vs) (96:ss) xs
generatedState96 vs ss (x@(AbsSynResult13 _):xs) = generatedState97 (x:vs) (96:ss) xs
generatedState96 vs ss xs = generatedError 96 xs


generatedState97 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction2 vs
generatedState97 vs ss xs = generatedError 97 xs


generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetypes"))):xs) = generatedState13 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynResult10 _):xs) = generatedState99 (x:vs) (98:ss) xs
generatedState98 vs ss xs = generatedError 98 xs


generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "state"))):xs) = generatedState18 (x:vs) (99:ss) xs
generatedState99 vs ss (x@(AbsSynResult12 _):xs) = generatedState100 (x:vs) (99:ss) xs
generatedState99 vs ss xs = generatedError 99 xs


generatedState100 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState25 (x:vs) (100:ss) xs
generatedState100 vs ss (x@(AbsSynResult16 _):xs) = generatedState27 (x:vs) (100:ss) xs
generatedState100 vs ss (x@(AbsSynResult13 _):xs) = generatedState101 (x:vs) (100:ss) xs
generatedState100 vs ss xs = generatedError 100 xs


generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction1 vs
generatedState101 vs ss xs = generatedError 101 xs


generatedState102 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState102 vs ss xs = generatedError 102 xs


generatedReduction1 ((AbsSynResult13 v6):(AbsSynResult12 v5):(AbsSynResult10 v4):(AbsSynResult5 v3):(AbsSynResult3 v2):(AbsSynResult6 v1):_) = AbsSynResult1 ((v1, v3, uncurry (SemanticsDef (fromMaybe "" v2) v4 v5) $ handleStatements v6))

generatedReduction2 ((AbsSynResult13 v6):(AbsSynResult12 v5):(AbsSynResult10 v4):(AbsSynResult4 v3):(AbsSynResult3 v2):(AbsSynResult6 v1):_) = AbsSynResult1 ((v1, v3, uncurry (SemanticsDef (fromMaybe "" v2) v4 v5) $ handleStatements v6))

generatedReduction3 ((AbsSynResult13 v6):(AbsSynResult12 v5):(AbsSynResult10 v4):(AbsSynResult5 v3):(AbsSynResult2 v2):(AbsSynResult6 v1):_) = AbsSynResult1 ((v1, v3, uncurry (SemanticsDef (fromMaybe "" v2) v4 v5) $ handleStatements v6))

generatedReduction4 ((AbsSynResult13 v6):(AbsSynResult12 v5):(AbsSynResult10 v4):(AbsSynResult4 v3):(AbsSynResult2 v2):(AbsSynResult6 v1):_) = AbsSynResult1 ((v1, v3, uncurry (SemanticsDef (fromMaybe "" v2) v4 v5) $ handleStatements v6))

generatedReduction5 (_) = AbsSynResult2 (empty)

generatedReduction6 ((AbsSynResult7 v1):_) = AbsSynResult3 (Just v1)

generatedReduction7 (_) = AbsSynResult4 (empty)

generatedReduction8 ((AbsSynResult8 v1):_) = AbsSynResult5 (Just v1)

generatedReduction9 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult6 (v2)

generatedReduction10 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult7 (v2)

generatedReduction11 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v2)

generatedReduction12 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult8 (v2)

generatedReduction13 ((AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult9 (v1)

generatedReduction14 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult9 (v1)

generatedReduction15 ((AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult9 (v1)

generatedReduction16 ((AbsSynResult11 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult10 (v2)

generatedReduction17 ((AbsSynResult11 v2):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult11 (v1:v2)

generatedReduction18 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult11 ([v1])

generatedReduction19 ((AbsSynResult9 v3):(AbsSynResult9 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 ((v2, v3))

generatedReduction20 ((AbsSynResult14 v2):(AbsSynResult16 v1):_) = AbsSynResult13 ((Right v1):v2)

generatedReduction21 ((AbsSynResult14 v2):(AbsSynResult15 v1):_) = AbsSynResult14 (v1:v2)

generatedReduction22 ((AbsSynResult15 v1):_) = AbsSynResult14 ([v1])

generatedReduction23 ((AbsSynResult17 v1):_) = AbsSynResult15 (Left v1)

generatedReduction24 ((AbsSynResult16 v1):_) = AbsSynResult15 (Right v1)

generatedReduction25 ((AbsSynToken (Token ps2 (TokenUpperIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult16 (v2)

generatedReduction26 ((AbsSynResult19 v7):(AbsSynResult21 v6):(AbsSynResult20 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (uncurry (SemanticsRule v2 v4 False v5 "" v7) v6)

generatedReduction27 ((AbsSynResult18 v7):(AbsSynResult21 v6):(AbsSynResult20 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (uncurry (SemanticsRule v2 v4 False v5 "" v7) v6)

generatedReduction28 ((AbsSynResult19 v7):(AbsSynResult21 v6):(AbsSynResult20 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (uncurry (SemanticsRule v2 v4 True v5 "" v7) v6)

generatedReduction29 ((AbsSynResult18 v7):(AbsSynResult21 v6):(AbsSynResult20 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 (uncurry (SemanticsRule v2 v4 True v5 "" v7) v6)

generatedReduction30 (_) = AbsSynResult18 (empty)

generatedReduction31 ((AbsSynResult28 v1):_) = AbsSynResult19 (Just v1)

generatedReduction32 ((AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult20 (SemanticsBaseType v2)

generatedReduction33 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult20 (SemanticsVarType v2)

generatedReduction34 (_) = AbsSynResult20 (SemanticsCommandType)

generatedReduction35 ((AbsSynResult24 v3):(AbsSynResult22 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult21 ((v2, v3))

generatedReduction36 (_) = AbsSynResult21 (([], []))

generatedReduction37 ((AbsSynResult22 v2):(AbsSynResult23 v1):_) = AbsSynResult22 (v1:v2)

generatedReduction38 ((AbsSynResult23 v1):_) = AbsSynResult22 ([v1])

generatedReduction39 ((AbsSynResult20 v4):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult23 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) False SemanticsDepSingle)

generatedReduction40 ((AbsSynResult20 v4):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult23 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) False (SemanticsDepFold False))

generatedReduction41 ((AbsSynResult20 v4):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult23 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) False (SemanticsDepFold True))

generatedReduction42 ((AbsSynResult20 v4):(AbsSynToken (Token ps3 (TokenCustom "CodeBlock" v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult23 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) True  SemanticsDepSingle)

generatedReduction43 ((AbsSynResult20 v4):(AbsSynToken (Token ps3 (TokenCustom "CodeBlock" v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult23 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) True  (SemanticsDepFold False))

generatedReduction44 ((AbsSynResult20 v4):(AbsSynToken (Token ps3 (TokenCustom "CodeBlock" v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult23 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) True  (SemanticsDepFold True))

generatedReduction45 ((AbsSynResult25 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult24 (v2)

generatedReduction46 (_) = AbsSynResult24 ([])

generatedReduction47 ((AbsSynResult25 v2):(AbsSynResult26 v1):_) = AbsSynResult25 (v1:v2)

generatedReduction48 ((AbsSynResult26 v1):_) = AbsSynResult25 ([v1])

generatedReduction49 ((AbsSynResult27 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult26 (SemanticsTypeRestriction v1 v3)

generatedReduction50 ((AbsSynResult27 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult27 (v1:v3)

generatedReduction51 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult27 ([v1])

generatedReduction52 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult28 (v2)

