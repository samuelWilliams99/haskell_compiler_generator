module Semanticsparser (runParser, module ParserRequirements) where

import ParserRequirements
import Control.Applicative
import Data.Maybe
import Data.Either
import Data.List
import Semantics
import Control.Arrow
import Data.HashMap.Strict hiding (empty, union)
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

generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14, generatedState15, generatedState16, generatedState17, generatedState18, generatedState19, generatedState20, generatedState21, generatedState22, generatedState23, generatedState24, generatedState25, generatedState26, generatedState27, generatedState28, generatedState29, generatedState30, generatedState31, generatedState32, generatedState33, generatedState34, generatedState35, generatedState36, generatedState37, generatedState38, generatedState39, generatedState40, generatedState41, generatedState42, generatedState43, generatedState44, generatedState45, generatedState46, generatedState47, generatedState48, generatedState49, generatedState50, generatedState51, generatedState52, generatedState53, generatedState54, generatedState55, generatedState56, generatedState57, generatedState58, generatedState59, generatedState60, generatedState61, generatedState62, generatedState63, generatedState64, generatedState65, generatedState66, generatedState67, generatedState68, generatedState69, generatedState70, generatedState71, generatedState72, generatedState73, generatedState74, generatedState75, generatedState76, generatedState77, generatedState78, generatedState79, generatedState80, generatedState81, generatedState82, generatedState83, generatedState84, generatedState85, generatedState86, generatedState87, generatedState88, generatedState89, generatedState90, generatedState91, generatedState92, generatedState93, generatedState94, generatedState95, generatedState96, generatedState97, generatedState98, generatedState99, generatedState100, generatedState101, generatedState102, generatedState103, generatedState104, generatedState105, generatedState106, generatedState107, generatedState108, generatedState109, generatedState110, generatedState111, generatedState112, generatedState113, generatedState114, generatedState115, generatedState116, generatedState117, generatedState118, generatedState119, generatedState120, generatedState121, generatedState122, generatedState123, generatedState124, generatedState125, generatedState126, generatedState127, generatedState128, generatedState129, generatedState130, generatedState131]

data AbsSynToken t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 = AbsSynToken Token | AbsSynResult1 t1 | AbsSynResult2 t2 | AbsSynResult3 t3 | AbsSynResult4 t4 | AbsSynResult5 t5 | AbsSynResult6 t6 | AbsSynResult7 t7 | AbsSynResult8 t8 | AbsSynResult9 t9 | AbsSynResult10 t10 | AbsSynResult11 t11 | AbsSynResult12 t12 | AbsSynResult13 t13 | AbsSynResult14 t14 | AbsSynResult15 t15 | AbsSynResult16 t16 | AbsSynResult17 t17 | AbsSynResult18 t18 | AbsSynResult19 t19 | AbsSynResult20 t20 | AbsSynResult21 t21 | AbsSynResult22 t22 | AbsSynResult23 t23 | AbsSynResult24 t24 | AbsSynResult25 t25 | AbsSynResult26 t26 | AbsSynResult27 t27 | AbsSynResult28 t28 | AbsSynResult29 t29 | AbsSynResult30 t30 | AbsSynResult31 t31 | AbsSynResult32 t32 | AbsSynResult33 t33 | AbsSynResult34 t34 | AbsSynResult35 t35 | AbsSynResult36 t36 | AbsSynResult37 t37

generatedState0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "extension"))):xs) = generatedState1 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult12 _):xs) = generatedState3 (x:vs) (0:ss) xs
generatedState0 vs ss (x@(AbsSynResult1 _):xs) = generatedState131 (x:vs) (0:ss) xs
generatedState0 vs ss xs = generatedError 0 xs


generatedState1 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState2 (x:vs) (1:ss) xs
generatedState1 vs ss xs = generatedError 1 xs


generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState2 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "importscode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 vs
generatedState2 vs ss xs = generatedError 2 xs


generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "importscode"))):xs) = generatedState4 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState3 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState3 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState3 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState3 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = generatedState3 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 vs
generatedState3 vs ss (x@(AbsSynResult13 _):xs) = generatedState6 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynResult2 _):xs) = generatedState7 (x:vs) (3:ss) xs
generatedState3 vs ss (x@(AbsSynResult3 _):xs) = generatedState120 (x:vs) (3:ss) xs
generatedState3 vs ss xs = generatedError 3 xs


generatedState4 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState5 (x:vs) (4:ss) xs
generatedState4 vs ss xs = generatedError 4 xs


generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState5 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 vs
generatedState5 vs ss xs = generatedError 5 xs


generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState6 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 vs
generatedState6 vs ss xs = generatedError 6 xs


generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = generatedState8 (x:vs) (7:ss) xs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState7 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState7 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState7 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState7 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState7 vs ss (x@(AbsSynResult14 _):xs) = generatedState10 (x:vs) (7:ss) xs
generatedState7 vs ss (x@(AbsSynResult4 _):xs) = generatedState11 (x:vs) (7:ss) xs
generatedState7 vs ss (x@(AbsSynResult5 _):xs) = generatedState115 (x:vs) (7:ss) xs
generatedState7 vs ss xs = generatedError 7 xs


generatedState8 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState9 (x:vs) (8:ss) xs
generatedState8 vs ss xs = generatedError 8 xs


generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState9 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction24 vs
generatedState9 vs ss xs = generatedError 9 xs


generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 vs
generatedState10 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 vs
generatedState10 vs ss xs = generatedError 10 xs


generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 (x:vs) (11:ss) xs
generatedState11 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState11 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState11 vs ss (x@(AbsSynResult15 _):xs) = generatedState15 (x:vs) (11:ss) xs
generatedState11 vs ss (x@(AbsSynResult6 _):xs) = generatedState16 (x:vs) (11:ss) xs
generatedState11 vs ss (x@(AbsSynResult7 _):xs) = generatedState113 (x:vs) (11:ss) xs
generatedState11 vs ss xs = generatedError 11 xs


generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState13 (x:vs) (12:ss) xs
generatedState12 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState14 (x:vs) (12:ss) xs
generatedState12 vs ss xs = generatedError 12 xs


generatedState13 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction26 vs
generatedState13 vs ss xs = generatedError 13 xs


generatedState14 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction25 vs
generatedState14 vs ss xs = generatedError 14 xs


generatedState15 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 vs
generatedState15 vs ss xs = generatedError 15 xs


generatedState16 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (16:ss) xs
generatedState16 vs ss (x@(AbsSynResult8 _):xs) = generatedState112 (x:vs) (16:ss) xs
generatedState16 vs ss xs = generatedError 16 xs


generatedState17 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState18 (x:vs) (17:ss) xs
generatedState17 vs ss xs = generatedError 17 xs


generatedState18 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState19 (x:vs) (18:ss) xs
generatedState18 vs ss xs = generatedError 18 xs


generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState19 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction30 vs
generatedState19 vs ss xs = generatedError 19 xs


generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (20:ss) xs
generatedState20 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (20:ss) xs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState20 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 vs
generatedState20 vs ss (x@(AbsSynResult9 _):xs) = generatedState21 (x:vs) (20:ss) xs
generatedState20 vs ss xs = generatedError 20 xs


generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState21 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 vs
generatedState21 vs ss xs = generatedError 21 xs


generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = generatedState23 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState22 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = generatedState22 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState22 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState22 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState22 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 vs
generatedState22 vs ss (x@(AbsSynResult18 _):xs) = generatedState26 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynResult10 _):xs) = generatedState28 (x:vs) (22:ss) xs
generatedState22 vs ss (x@(AbsSynResult11 _):xs) = generatedState107 (x:vs) (22:ss) xs
generatedState22 vs ss xs = generatedError 22 xs


generatedState23 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState24 (x:vs) (23:ss) xs
generatedState23 vs ss xs = generatedError 23 xs


generatedState24 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState25 (x:vs) (24:ss) xs
generatedState24 vs ss xs = generatedError 24 xs


generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState25 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction31 vs
generatedState25 vs ss xs = generatedError 25 xs


generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = generatedState23 (x:vs) (26:ss) xs
generatedState26 vs ss (x@(AbsSynResult18 _):xs) = generatedState26 (x:vs) (26:ss) xs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 vs
generatedState26 vs ss (x@(AbsSynResult11 _):xs) = generatedState27 (x:vs) (26:ss) xs
generatedState26 vs ss xs = generatedError 26 xs


generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState27 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 vs
generatedState27 vs ss xs = generatedError 27 xs


generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState28 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState28 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState28 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState28 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = generatedState29 (x:vs) (28:ss) xs
generatedState28 vs ss (x@(AbsSynResult19 _):xs) = generatedState35 (x:vs) (28:ss) xs
generatedState28 vs ss xs = generatedError 28 xs


generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState30 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState31 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState32 (x:vs) (29:ss) xs
generatedState29 vs ss (x@(AbsSynResult16 _):xs) = generatedState33 (x:vs) (29:ss) xs
generatedState29 vs ss xs = generatedError 29 xs


generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState30 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 vs
generatedState30 vs ss xs = generatedError 30 xs


generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState31 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 vs
generatedState31 vs ss xs = generatedError 31 xs


generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState32 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 vs
generatedState32 vs ss xs = generatedError 32 xs


generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState30 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState31 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState32 (x:vs) (33:ss) xs
generatedState33 vs ss (x@(AbsSynResult16 _):xs) = generatedState34 (x:vs) (33:ss) xs
generatedState33 vs ss xs = generatedError 33 xs


generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction32 vs
generatedState34 vs ss xs = generatedError 34 xs


generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState35 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState35 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState36 (x:vs) (35:ss) xs
generatedState35 vs ss (x@(AbsSynResult20 _):xs) = generatedState38 (x:vs) (35:ss) xs
generatedState35 vs ss xs = generatedError 35 xs


generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState30 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState31 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState32 (x:vs) (36:ss) xs
generatedState36 vs ss (x@(AbsSynResult16 _):xs) = generatedState37 (x:vs) (36:ss) xs
generatedState36 vs ss xs = generatedError 36 xs


generatedState37 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction34 vs
generatedState37 vs ss xs = generatedError 37 xs


generatedState38 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState39 (x:vs) (38:ss) xs
generatedState38 vs ss (x@(AbsSynResult21 _):xs) = generatedState41 (x:vs) (38:ss) xs
generatedState38 vs ss xs = generatedError 38 xs


generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState30 (x:vs) (39:ss) xs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState31 (x:vs) (39:ss) xs
generatedState39 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState32 (x:vs) (39:ss) xs
generatedState39 vs ss (x@(AbsSynResult16 _):xs) = generatedState40 (x:vs) (39:ss) xs
generatedState39 vs ss xs = generatedError 39 xs


generatedState40 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction36 vs
generatedState40 vs ss xs = generatedError 40 xs


generatedState41 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState42 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynResult25 _):xs) = generatedState44 (x:vs) (41:ss) xs
generatedState41 vs ss (x@(AbsSynResult22 _):xs) = generatedState106 (x:vs) (41:ss) xs
generatedState41 vs ss xs = generatedError 41 xs


generatedState42 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState43 (x:vs) (42:ss) xs
generatedState42 vs ss xs = generatedError 42 xs


generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState43 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction42 vs
generatedState43 vs ss xs = generatedError 43 xs


generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState42 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState45 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynResult25 _):xs) = generatedState101 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynResult26 _):xs) = generatedState102 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynResult24 _):xs) = generatedState103 (x:vs) (44:ss) xs
generatedState44 vs ss (x@(AbsSynResult23 _):xs) = generatedState105 (x:vs) (44:ss) xs
generatedState44 vs ss xs = generatedError 44 xs


generatedState45 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState46 (x:vs) (45:ss) xs
generatedState45 vs ss xs = generatedError 45 xs


generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState47 (x:vs) (46:ss) xs
generatedState46 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState95 (x:vs) (46:ss) xs
generatedState46 vs ss xs = generatedError 46 xs


generatedState47 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState48 (x:vs) (47:ss) xs
generatedState47 vs ss xs = generatedError 47 xs


generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState48 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState48 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (48:ss) xs
generatedState48 vs ss (x@(AbsSynResult29 _):xs) = generatedState53 (x:vs) (48:ss) xs
generatedState48 vs ss xs = generatedError 48 xs


generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState50 (x:vs) (49:ss) xs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState51 (x:vs) (49:ss) xs
generatedState49 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState52 (x:vs) (49:ss) xs
generatedState49 vs ss xs = generatedError 49 xs


generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction51 vs
generatedState50 vs ss xs = generatedError 50 xs


generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction50 vs
generatedState51 vs ss xs = generatedError 51 xs


generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction49 vs
generatedState52 vs ss xs = generatedError 52 xs


generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState53 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState53 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState53 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState53 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState53 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState54 (x:vs) (53:ss) xs
generatedState53 vs ss (x@(AbsSynResult30 _):xs) = generatedState89 (x:vs) (53:ss) xs
generatedState53 vs ss xs = generatedError 53 xs


generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState55 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState65 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynResult32 _):xs) = generatedState75 (x:vs) (54:ss) xs
generatedState54 vs ss (x@(AbsSynResult31 _):xs) = generatedState77 (x:vs) (54:ss) xs
generatedState54 vs ss xs = generatedError 54 xs


generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^=>"))):xs) = generatedState56 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*=>"))):xs) = generatedState59 (x:vs) (55:ss) xs
generatedState55 vs ss (x@(AbsSynToken (Token ps (TokenOperator "=>"))):xs) = generatedState62 (x:vs) (55:ss) xs
generatedState55 vs ss xs = generatedError 55 xs


generatedState56 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState57 (x:vs) (56:ss) xs
generatedState56 vs ss xs = generatedError 56 xs


generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState57 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState57 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (57:ss) xs
generatedState57 vs ss (x@(AbsSynResult29 _):xs) = generatedState58 (x:vs) (57:ss) xs
generatedState57 vs ss xs = generatedError 57 xs


generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState58 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction62 vs
generatedState58 vs ss xs = generatedError 58 xs


generatedState59 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState60 (x:vs) (59:ss) xs
generatedState59 vs ss xs = generatedError 59 xs


generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState60 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState60 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (60:ss) xs
generatedState60 vs ss (x@(AbsSynResult29 _):xs) = generatedState61 (x:vs) (60:ss) xs
generatedState60 vs ss xs = generatedError 60 xs


generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState61 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction61 vs
generatedState61 vs ss xs = generatedError 61 xs


generatedState62 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState63 (x:vs) (62:ss) xs
generatedState62 vs ss xs = generatedError 62 xs


generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState63 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState63 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (63:ss) xs
generatedState63 vs ss (x@(AbsSynResult29 _):xs) = generatedState64 (x:vs) (63:ss) xs
generatedState63 vs ss xs = generatedError 63 xs


generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState64 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction60 vs
generatedState64 vs ss xs = generatedError 64 xs


generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = generatedState66 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = generatedState69 (x:vs) (65:ss) xs
generatedState65 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState72 (x:vs) (65:ss) xs
generatedState65 vs ss xs = generatedError 65 xs


generatedState66 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState67 (x:vs) (66:ss) xs
generatedState66 vs ss xs = generatedError 66 xs


generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState67 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState67 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (67:ss) xs
generatedState67 vs ss (x@(AbsSynResult29 _):xs) = generatedState68 (x:vs) (67:ss) xs
generatedState67 vs ss xs = generatedError 67 xs


generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState68 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction59 vs
generatedState68 vs ss xs = generatedError 68 xs


generatedState69 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState70 (x:vs) (69:ss) xs
generatedState69 vs ss xs = generatedError 69 xs


generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState70 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState70 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (70:ss) xs
generatedState70 vs ss (x@(AbsSynResult29 _):xs) = generatedState71 (x:vs) (70:ss) xs
generatedState70 vs ss xs = generatedError 70 xs


generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState71 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction58 vs
generatedState71 vs ss xs = generatedError 71 xs


generatedState72 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState73 (x:vs) (72:ss) xs
generatedState72 vs ss xs = generatedError 72 xs


generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState73 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState73 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (73:ss) xs
generatedState73 vs ss (x@(AbsSynResult29 _):xs) = generatedState74 (x:vs) (73:ss) xs
generatedState73 vs ss xs = generatedError 73 xs


generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState74 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 3)) (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction57 vs
generatedState74 vs ss xs = generatedError 74 xs


generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState55 (x:vs) (75:ss) xs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState65 (x:vs) (75:ss) xs
generatedState75 vs ss (x@(AbsSynResult32 _):xs) = generatedState75 (x:vs) (75:ss) xs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState75 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction56 vs
generatedState75 vs ss (x@(AbsSynResult31 _):xs) = generatedState76 (x:vs) (75:ss) xs
generatedState75 vs ss xs = generatedError 75 xs


generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState76 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 vs
generatedState76 vs ss xs = generatedError 76 xs


generatedState77 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState77 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState77 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState77 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState77 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState77 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState77 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState77 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 vs
generatedState77 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState78 (x:vs) (77:ss) xs
generatedState77 vs ss (x@(AbsSynResult33 _):xs) = generatedState88 (x:vs) (77:ss) xs
generatedState77 vs ss xs = generatedError 77 xs


generatedState78 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState79 (x:vs) (78:ss) xs
generatedState78 vs ss (x@(AbsSynResult35 _):xs) = generatedState85 (x:vs) (78:ss) xs
generatedState78 vs ss (x@(AbsSynResult34 _):xs) = generatedState87 (x:vs) (78:ss) xs
generatedState78 vs ss xs = generatedError 78 xs


generatedState79 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "to"))):xs) = generatedState80 (x:vs) (79:ss) xs
generatedState79 vs ss xs = generatedError 79 xs


generatedState80 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState81 (x:vs) (80:ss) xs
generatedState80 vs ss (x@(AbsSynResult36 _):xs) = generatedState84 (x:vs) (80:ss) xs
generatedState80 vs ss xs = generatedError 80 xs


generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction69 vs
generatedState81 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = generatedState82 (x:vs) (81:ss) xs
generatedState81 vs ss xs = generatedError 81 xs


generatedState82 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState81 (x:vs) (82:ss) xs
generatedState82 vs ss (x@(AbsSynResult36 _):xs) = generatedState83 (x:vs) (82:ss) xs
generatedState82 vs ss xs = generatedError 82 xs


generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState83 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction68 vs
generatedState83 vs ss xs = generatedError 83 xs


generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState84 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction67 vs
generatedState84 vs ss xs = generatedError 84 xs


generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState79 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynResult35 _):xs) = generatedState85 (x:vs) (85:ss) xs
generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState85 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 vs
generatedState85 vs ss (x@(AbsSynResult34 _):xs) = generatedState86 (x:vs) (85:ss) xs
generatedState85 vs ss xs = generatedError 85 xs


generatedState86 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState86 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState86 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState86 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction65 vs
generatedState86 vs ss xs = generatedError 86 xs


generatedState87 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState87 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState87 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState87 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 vs
generatedState87 vs ss xs = generatedError 87 xs


generatedState88 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState88 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState88 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState88 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction53 vs
generatedState88 vs ss xs = generatedError 88 xs


generatedState89 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState90 (x:vs) (89:ss) xs
generatedState89 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState89 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState89 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState89 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState89 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState89 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState89 vs ss (x@(AbsSynResult37 _):xs) = generatedState92 (x:vs) (89:ss) xs
generatedState89 vs ss (x@(AbsSynResult27 _):xs) = generatedState93 (x:vs) (89:ss) xs
generatedState89 vs ss (x@(AbsSynResult28 _):xs) = generatedState94 (x:vs) (89:ss) xs
generatedState89 vs ss xs = generatedError 89 xs


generatedState90 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState91 (x:vs) (90:ss) xs
generatedState90 vs ss xs = generatedError 90 xs


generatedState91 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState91 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState91 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction70 vs
generatedState91 vs ss xs = generatedError 91 xs


generatedState92 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState92 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState92 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction48 vs
generatedState92 vs ss xs = generatedError 92 xs


generatedState93 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState93 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState93 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction46 vs
generatedState93 vs ss xs = generatedError 93 xs


generatedState94 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState94 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState94 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction45 vs
generatedState94 vs ss xs = generatedError 94 xs


generatedState95 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState96 (x:vs) (95:ss) xs
generatedState95 vs ss xs = generatedError 95 xs


generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState96 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction52 vs
generatedState96 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState49 (x:vs) (96:ss) xs
generatedState96 vs ss (x@(AbsSynResult29 _):xs) = generatedState97 (x:vs) (96:ss) xs
generatedState96 vs ss xs = generatedError 96 xs


generatedState97 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState97 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState97 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState97 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState97 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState97 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState97 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState97 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction54 vs
generatedState97 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState54 (x:vs) (97:ss) xs
generatedState97 vs ss (x@(AbsSynResult30 _):xs) = generatedState98 (x:vs) (97:ss) xs
generatedState97 vs ss xs = generatedError 97 xs


generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState90 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState98 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState98 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction47 vs
generatedState98 vs ss (x@(AbsSynResult37 _):xs) = generatedState92 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynResult27 _):xs) = generatedState99 (x:vs) (98:ss) xs
generatedState98 vs ss (x@(AbsSynResult28 _):xs) = generatedState100 (x:vs) (98:ss) xs
generatedState98 vs ss xs = generatedError 98 xs


generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState99 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction44 vs
generatedState99 vs ss xs = generatedError 99 xs


generatedState100 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState100 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState100 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 6)) (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction43 vs
generatedState100 vs ss xs = generatedError 100 xs


generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState101 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 vs
generatedState101 vs ss xs = generatedError 101 xs


generatedState102 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState102 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState102 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction40 vs
generatedState102 vs ss xs = generatedError 102 xs


generatedState103 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState42 (x:vs) (103:ss) xs
generatedState103 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState45 (x:vs) (103:ss) xs
generatedState103 vs ss (x@(AbsSynResult25 _):xs) = generatedState101 (x:vs) (103:ss) xs
generatedState103 vs ss (x@(AbsSynResult26 _):xs) = generatedState102 (x:vs) (103:ss) xs
generatedState103 vs ss (x@(AbsSynResult24 _):xs) = generatedState103 (x:vs) (103:ss) xs
generatedState103 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction39 vs
generatedState103 vs ss (x@(AbsSynResult23 _):xs) = generatedState104 (x:vs) (103:ss) xs
generatedState103 vs ss xs = generatedError 103 xs


generatedState104 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction38 vs
generatedState104 vs ss xs = generatedError 104 xs


generatedState105 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 vs
generatedState105 vs ss xs = generatedError 105 xs


generatedState106 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction16 vs
generatedState106 vs ss xs = generatedError 106 xs


generatedState107 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState107 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState107 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState107 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState107 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState107 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction33 vs
generatedState107 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = generatedState29 (x:vs) (107:ss) xs
generatedState107 vs ss (x@(AbsSynResult19 _):xs) = generatedState108 (x:vs) (107:ss) xs
generatedState107 vs ss xs = generatedError 107 xs


generatedState108 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState108 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction35 vs
generatedState108 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState36 (x:vs) (108:ss) xs
generatedState108 vs ss (x@(AbsSynResult20 _):xs) = generatedState109 (x:vs) (108:ss) xs
generatedState108 vs ss xs = generatedError 108 xs


generatedState109 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState39 (x:vs) (109:ss) xs
generatedState109 vs ss (x@(AbsSynResult21 _):xs) = generatedState110 (x:vs) (109:ss) xs
generatedState109 vs ss xs = generatedError 109 xs


generatedState110 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState42 (x:vs) (110:ss) xs
generatedState110 vs ss (x@(AbsSynResult25 _):xs) = generatedState44 (x:vs) (110:ss) xs
generatedState110 vs ss (x@(AbsSynResult22 _):xs) = generatedState111 (x:vs) (110:ss) xs
generatedState110 vs ss xs = generatedError 110 xs


generatedState111 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction15 vs
generatedState111 vs ss xs = generatedError 111 xs


generatedState112 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction8 vs
generatedState112 vs ss xs = generatedError 112 xs


generatedState113 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (113:ss) xs
generatedState113 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (113:ss) xs
generatedState113 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (113:ss) xs
generatedState113 vs ss (x@(AbsSynResult8 _):xs) = generatedState114 (x:vs) (113:ss) xs
generatedState113 vs ss xs = generatedError 113 xs


generatedState114 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction7 vs
generatedState114 vs ss xs = generatedError 114 xs


generatedState115 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 (x:vs) (115:ss) xs
generatedState115 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState115 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState115 vs ss (x@(AbsSynResult15 _):xs) = generatedState15 (x:vs) (115:ss) xs
generatedState115 vs ss (x@(AbsSynResult6 _):xs) = generatedState116 (x:vs) (115:ss) xs
generatedState115 vs ss (x@(AbsSynResult7 _):xs) = generatedState118 (x:vs) (115:ss) xs
generatedState115 vs ss xs = generatedError 115 xs


generatedState116 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (116:ss) xs
generatedState116 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (116:ss) xs
generatedState116 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (116:ss) xs
generatedState116 vs ss (x@(AbsSynResult8 _):xs) = generatedState117 (x:vs) (116:ss) xs
generatedState116 vs ss xs = generatedError 116 xs


generatedState117 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction6 vs
generatedState117 vs ss xs = generatedError 117 xs


generatedState118 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (118:ss) xs
generatedState118 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (118:ss) xs
generatedState118 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (118:ss) xs
generatedState118 vs ss (x@(AbsSynResult8 _):xs) = generatedState119 (x:vs) (118:ss) xs
generatedState118 vs ss xs = generatedError 118 xs


generatedState119 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction5 vs
generatedState119 vs ss xs = generatedError 119 xs


generatedState120 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = generatedState8 (x:vs) (120:ss) xs
generatedState120 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState120 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState120 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState120 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState120 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState120 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 vs
generatedState120 vs ss (x@(AbsSynResult14 _):xs) = generatedState10 (x:vs) (120:ss) xs
generatedState120 vs ss (x@(AbsSynResult4 _):xs) = generatedState121 (x:vs) (120:ss) xs
generatedState120 vs ss (x@(AbsSynResult5 _):xs) = generatedState126 (x:vs) (120:ss) xs
generatedState120 vs ss xs = generatedError 120 xs


generatedState121 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 (x:vs) (121:ss) xs
generatedState121 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState121 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState121 vs ss (x@(AbsSynResult15 _):xs) = generatedState15 (x:vs) (121:ss) xs
generatedState121 vs ss (x@(AbsSynResult6 _):xs) = generatedState122 (x:vs) (121:ss) xs
generatedState121 vs ss (x@(AbsSynResult7 _):xs) = generatedState124 (x:vs) (121:ss) xs
generatedState121 vs ss xs = generatedError 121 xs


generatedState122 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (122:ss) xs
generatedState122 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (122:ss) xs
generatedState122 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (122:ss) xs
generatedState122 vs ss (x@(AbsSynResult8 _):xs) = generatedState123 (x:vs) (122:ss) xs
generatedState122 vs ss xs = generatedError 122 xs


generatedState123 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction4 vs
generatedState123 vs ss xs = generatedError 123 xs


generatedState124 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (124:ss) xs
generatedState124 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (124:ss) xs
generatedState124 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (124:ss) xs
generatedState124 vs ss (x@(AbsSynResult8 _):xs) = generatedState125 (x:vs) (124:ss) xs
generatedState124 vs ss xs = generatedError 124 xs


generatedState125 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction3 vs
generatedState125 vs ss xs = generatedError 125 xs


generatedState126 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 (x:vs) (126:ss) xs
generatedState126 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState126 (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 vs
generatedState126 vs ss (x@(AbsSynResult15 _):xs) = generatedState15 (x:vs) (126:ss) xs
generatedState126 vs ss (x@(AbsSynResult6 _):xs) = generatedState127 (x:vs) (126:ss) xs
generatedState126 vs ss (x@(AbsSynResult7 _):xs) = generatedState129 (x:vs) (126:ss) xs
generatedState126 vs ss xs = generatedError 126 xs


generatedState127 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (127:ss) xs
generatedState127 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (127:ss) xs
generatedState127 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (127:ss) xs
generatedState127 vs ss (x@(AbsSynResult8 _):xs) = generatedState128 (x:vs) (127:ss) xs
generatedState127 vs ss xs = generatedError 127 xs


generatedState128 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction2 vs
generatedState128 vs ss xs = generatedError 128 xs


generatedState129 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState17 (x:vs) (129:ss) xs
generatedState129 vs ss (x@(AbsSynResult17 _):xs) = generatedState20 (x:vs) (129:ss) xs
generatedState129 vs ss (x@(AbsSynResult9 _):xs) = generatedState22 (x:vs) (129:ss) xs
generatedState129 vs ss (x@(AbsSynResult8 _):xs) = generatedState130 (x:vs) (129:ss) xs
generatedState129 vs ss xs = generatedError 129 xs


generatedState130 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction1 vs
generatedState130 vs ss xs = generatedError 130 xs


generatedState131 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState131 vs ss xs = generatedError 131 xs


generatedReduction1 ((AbsSynResult8 v5):(AbsSynResult7 v4):(AbsSynResult5 v3):(AbsSynResult3 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction2 ((AbsSynResult8 v5):(AbsSynResult6 v4):(AbsSynResult5 v3):(AbsSynResult3 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction3 ((AbsSynResult8 v5):(AbsSynResult7 v4):(AbsSynResult4 v3):(AbsSynResult3 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction4 ((AbsSynResult8 v5):(AbsSynResult6 v4):(AbsSynResult4 v3):(AbsSynResult3 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction5 ((AbsSynResult8 v5):(AbsSynResult7 v4):(AbsSynResult5 v3):(AbsSynResult2 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction6 ((AbsSynResult8 v5):(AbsSynResult6 v4):(AbsSynResult5 v3):(AbsSynResult2 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction7 ((AbsSynResult8 v5):(AbsSynResult7 v4):(AbsSynResult4 v3):(AbsSynResult2 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction8 ((AbsSynResult8 v5):(AbsSynResult6 v4):(AbsSynResult4 v3):(AbsSynResult2 v2):(AbsSynResult12 v1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5))

generatedReduction9 (_) = AbsSynResult2 (empty)

generatedReduction10 ((AbsSynResult13 v1):_) = AbsSynResult3 (Just v1)

generatedReduction11 (_) = AbsSynResult4 (empty)

generatedReduction12 ((AbsSynResult14 v1):_) = AbsSynResult5 (Just v1)

generatedReduction13 (_) = AbsSynResult6 (empty)

generatedReduction14 ((AbsSynResult15 v1):_) = AbsSynResult7 (Just v1)

generatedReduction15 ((AbsSynResult22 v6):(AbsSynResult21 v5):(AbsSynResult20 v4):(AbsSynResult19 v3):(AbsSynResult11 v2):(AbsSynResult9 v1):_) = AbsSynResult8 (uncurry (SemanticsDef (fromList v1) (fromList v2) v3 v4 v5) $ handleStatements v6)

generatedReduction16 ((AbsSynResult22 v6):(AbsSynResult21 v5):(AbsSynResult20 v4):(AbsSynResult19 v3):(AbsSynResult10 v2):(AbsSynResult9 v1):_) = AbsSynResult8 (uncurry (SemanticsDef (fromList v1) (fromList v2) v3 v4 v5) $ handleStatements v6)

generatedReduction17 ((AbsSynResult9 v2):(AbsSynResult17 v1):_) = AbsSynResult9 (v1:v2)

generatedReduction18 ((AbsSynResult17 v1):_) = AbsSynResult9 ([v1])

generatedReduction19 (_) = AbsSynResult10 (empty)

generatedReduction20 ((AbsSynResult11 v2):(AbsSynResult18 v1):_) = AbsSynResult11 (v1:v2)

generatedReduction21 ((AbsSynResult18 v1):_) = AbsSynResult11 ([v1])

generatedReduction22 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v2)

generatedReduction23 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult13 (v2)

generatedReduction24 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult14 (v2)

generatedReduction25 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult15 (v2)

generatedReduction26 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult15 (v2)

generatedReduction27 ((AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult16 (v1)

generatedReduction28 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult16 (v1)

generatedReduction29 ((AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult16 (v1)

generatedReduction30 ((AbsSynToken (Token ps3 (TokenStringLit v3))):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult17 ((v2, v3))

generatedReduction31 ((AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult18 ((v2, v3))

generatedReduction32 ((AbsSynResult16 v3):(AbsSynResult16 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult19 ((v2, v3))

generatedReduction33 (_) = AbsSynResult19 (("()", "()"))

generatedReduction34 ((AbsSynResult16 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult20 (v2)

generatedReduction35 (_) = AbsSynResult20 ("()")

generatedReduction36 ((AbsSynResult16 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult21 (v2)

generatedReduction37 ((AbsSynResult23 v2):(AbsSynResult25 v1):_) = AbsSynResult22 ((Right v1):v2)

generatedReduction38 ((AbsSynResult23 v2):(AbsSynResult24 v1):_) = AbsSynResult23 (v1:v2)

generatedReduction39 ((AbsSynResult24 v1):_) = AbsSynResult23 ([v1])

generatedReduction40 ((AbsSynResult26 v1):_) = AbsSynResult24 (Left v1)

generatedReduction41 ((AbsSynResult25 v1):_) = AbsSynResult24 (Right v1)

generatedReduction42 ((AbsSynToken (Token ps2 (TokenUpperIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult25 (v2)

generatedReduction43 ((AbsSynResult28 v7):(AbsSynResult30 v6):(AbsSynResult29 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult26 (uncurry (SemanticsRule v2 v4 False v5 "" v7) v6)

generatedReduction44 ((AbsSynResult27 v7):(AbsSynResult30 v6):(AbsSynResult29 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult26 (uncurry (SemanticsRule v2 v4 False v5 "" v7) v6)

generatedReduction45 ((AbsSynResult28 v7):(AbsSynResult30 v6):(AbsSynResult29 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult26 (uncurry (SemanticsRule v2 v4 True v5 "" v7) v6)

generatedReduction46 ((AbsSynResult27 v7):(AbsSynResult30 v6):(AbsSynResult29 v5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult26 (uncurry (SemanticsRule v2 v4 True v5 "" v7) v6)

generatedReduction47 (_) = AbsSynResult27 (empty)

generatedReduction48 ((AbsSynResult37 v1):_) = AbsSynResult28 (Just v1)

generatedReduction49 ((AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult29 (SemanticsStaticBaseType v2)

generatedReduction50 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult29 (SemanticsStaticType v2)

generatedReduction51 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult29 (SemanticsVarType v2)

generatedReduction52 (_) = AbsSynResult29 (SemanticsCommandType)

generatedReduction53 ((AbsSynResult33 v3):(AbsSynResult31 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult30 ((v2, v3))

generatedReduction54 (_) = AbsSynResult30 (([], []))

generatedReduction55 ((AbsSynResult31 v2):(AbsSynResult32 v1):_) = AbsSynResult31 (v1:v2)

generatedReduction56 ((AbsSynResult32 v1):_) = AbsSynResult31 ([v1])

generatedReduction57 ((AbsSynResult29 v4):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult32 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) False SemanticsDepSingle)

generatedReduction58 ((AbsSynResult29 v4):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult32 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) False (SemanticsDepFold False))

generatedReduction59 ((AbsSynResult29 v4):(AbsSynToken (Token ps3 (TokenIdentifier v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult32 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) False (SemanticsDepFold True))

generatedReduction60 ((AbsSynResult29 v4):(AbsSynToken (Token ps3 (TokenCustom "CodeBlock" v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult32 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) True  SemanticsDepSingle)

generatedReduction61 ((AbsSynResult29 v4):(AbsSynToken (Token ps3 (TokenCustom "CodeBlock" v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult32 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) True  (SemanticsDepFold False))

generatedReduction62 ((AbsSynResult29 v4):(AbsSynToken (Token ps3 (TokenCustom "CodeBlock" v3))):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult32 (SemanticsRuleDependency v1 v3 (RawSemanticsDepType v4) True  (SemanticsDepFold True))

generatedReduction63 ((AbsSynResult34 v2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult33 (v2)

generatedReduction64 (_) = AbsSynResult33 ([])

generatedReduction65 ((AbsSynResult34 v2):(AbsSynResult35 v1):_) = AbsSynResult34 (v1:v2)

generatedReduction66 ((AbsSynResult35 v1):_) = AbsSynResult34 ([v1])

generatedReduction67 ((AbsSynResult36 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult35 (SemanticsTypeRestriction v1 v3)

generatedReduction68 ((AbsSynResult36 v3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult36 (v1:v3)

generatedReduction69 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult36 ([v1])

generatedReduction70 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult37 (v2)

