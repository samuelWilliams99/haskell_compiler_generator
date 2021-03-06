{-|
Module      : Semanticsparser
Description : Parser generated with the Haskell Parser Generator - https://github.com/samuelWilliams99/haskell_parser_generator
-}
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
                  , operators=["->","<-","@",",","*->","^->","~"]
                  , keywords=["evaluating","where","restricting","to","case"]
                  , lineComment=Just "#"
                  , blockComment=Just ("#[","]#")
                  , includeEOF=True
                  , parserMap=(languageDefsParser:)
                  }

-- | Generates the Abstract Syntax Tree from an input string, can fail
runParser str = do
    ts <- scan gScanner str
    let ps = if length ts == 0 then parseState "" else let (Token ps' _) = head ts in ps'
    generatedState0 ps [] [] $ fmap AbsSynToken ts

generatedError n [] = Error "Ran out of tokens"
generatedError n ((AbsSynToken (Token ps x)):xs) = Error $ "Unexpected token: " ++ (show x) ++ " at " ++ showPos ps

unpackFinal (AbsSynResult1 x _) = x


generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14, generatedState15, generatedState16, generatedState17, generatedState18, generatedState19, generatedState20, generatedState21, generatedState22, generatedState23, generatedState24, generatedState25, generatedState26, generatedState27, generatedState28, generatedState29, generatedState30, generatedState31, generatedState32, generatedState33, generatedState34, generatedState35, generatedState36, generatedState37, generatedState38, generatedState39, generatedState40, generatedState41, generatedState42, generatedState43, generatedState44, generatedState45, generatedState46, generatedState47, generatedState48, generatedState49, generatedState50, generatedState51, generatedState52, generatedState53, generatedState54, generatedState55, generatedState56, generatedState57, generatedState58, generatedState59, generatedState60, generatedState61, generatedState62, generatedState63, generatedState64, generatedState65, generatedState66, generatedState67, generatedState68, generatedState69, generatedState70, generatedState71, generatedState72, generatedState73, generatedState74, generatedState75, generatedState76, generatedState77, generatedState78, generatedState79, generatedState80, generatedState81, generatedState82, generatedState83, generatedState84, generatedState85, generatedState86, generatedState87, generatedState88, generatedState89, generatedState90, generatedState91, generatedState92, generatedState93, generatedState94, generatedState95, generatedState96, generatedState97, generatedState98, generatedState99, generatedState100, generatedState101, generatedState102, generatedState103, generatedState104, generatedState105, generatedState106, generatedState107, generatedState108, generatedState109, generatedState110, generatedState111, generatedState112, generatedState113, generatedState114, generatedState115, generatedState116, generatedState117, generatedState118, generatedState119, generatedState120, generatedState121, generatedState122, generatedState123]

data AbsSynToken t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 = AbsSynToken Token | AbsSynResult1 t1 ParseState | AbsSynResult2 t2 ParseState | AbsSynResult3 t3 ParseState | AbsSynResult4 t4 ParseState | AbsSynResult5 t5 ParseState | AbsSynResult6 t6 ParseState | AbsSynResult7 t7 ParseState | AbsSynResult8 t8 ParseState | AbsSynResult9 t9 ParseState | AbsSynResult10 t10 ParseState | AbsSynResult11 t11 ParseState | AbsSynResult12 t12 ParseState | AbsSynResult13 t13 ParseState | AbsSynResult14 t14 ParseState | AbsSynResult15 t15 ParseState | AbsSynResult16 t16 ParseState | AbsSynResult17 t17 ParseState | AbsSynResult18 t18 ParseState | AbsSynResult19 t19 ParseState | AbsSynResult20 t20 ParseState | AbsSynResult21 t21 ParseState | AbsSynResult22 t22 ParseState | AbsSynResult23 t23 ParseState | AbsSynResult24 t24 ParseState | AbsSynResult25 t25 ParseState | AbsSynResult26 t26 ParseState | AbsSynResult27 t27 ParseState | AbsSynResult28 t28 ParseState | AbsSynResult29 t29 ParseState | AbsSynResult30 t30 ParseState | AbsSynResult31 t31 ParseState | AbsSynResult32 t32 ParseState | AbsSynResult33 t33 ParseState | AbsSynResult34 t34 ParseState | AbsSynResult35 t35 ParseState | AbsSynResult36 t36 ParseState | AbsSynResult37 t37 ParseState | AbsSynResult38 t38 ParseState | AbsSynResult39 t39 ParseState | AbsSynResult40 t40 ParseState | AbsSynResult41 t41 ParseState | AbsSynResult42 t42 ParseState | AbsSynResult43 t43 ParseState

generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "extension"))):xs) = generatedState1 ps (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult12 _ _):xs) = generatedState3 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult1 _ _):xs) = generatedState123 ps0 (x:vs) (0:ss) xs
generatedState0 _ _ _ xs = generatedError 0 xs


generatedState1 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState2 ps (x:vs) (1:ss) xs
generatedState1 _ _ _ xs = generatedError 1 xs


generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "importscode"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction22 ps0 vs
generatedState2 _ _ _ xs = generatedError 2 xs


generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "importscode"))):xs) = generatedState4 ps (x:vs) (3:ss) xs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState3 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState3 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState3 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState3 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = generatedState3 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynResult13 _ _):xs) = generatedState6 ps0 (x:vs) (3:ss) xs
generatedState3 ps0 vs ss (x@(AbsSynResult2 _ _):xs) = generatedState7 ps0 (x:vs) (3:ss) xs
generatedState3 ps0 vs ss (x@(AbsSynResult3 _ _):xs) = generatedState112 ps0 (x:vs) (3:ss) xs
generatedState3 _ _ _ xs = generatedError 3 xs


generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState5 ps (x:vs) (4:ss) xs
generatedState4 _ _ _ xs = generatedError 4 xs


generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction23 ps0 vs
generatedState5 _ _ _ xs = generatedError 5 xs


generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction10 ps0 vs
generatedState6 _ _ _ xs = generatedError 6 xs


generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = generatedState8 ps (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState7 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState7 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState7 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState7 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState7 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState10 ps0 (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState11 ps0 (x:vs) (7:ss) xs
generatedState7 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState107 ps0 (x:vs) (7:ss) xs
generatedState7 _ _ _ xs = generatedError 7 xs


generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState9 ps (x:vs) (8:ss) xs
generatedState8 _ _ _ xs = generatedError 8 xs


generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction24 ps0 vs
generatedState9 _ _ _ xs = generatedError 9 xs


generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction12 ps0 vs
generatedState10 _ _ _ xs = generatedError 10 xs


generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 ps (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState11 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState11 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState11 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState17 ps0 (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState18 ps0 (x:vs) (11:ss) xs
generatedState11 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState105 ps0 (x:vs) (11:ss) xs
generatedState11 _ _ _ xs = generatedError 11 xs


generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (12:ss) xs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (12:ss) xs
generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (12:ss) xs
generatedState12 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState16 ps0 (x:vs) (12:ss) xs
generatedState12 _ _ _ xs = generatedError 12 xs


generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction28 ps0 vs
generatedState13 _ _ _ xs = generatedError 13 xs


generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction27 ps0 vs
generatedState14 _ _ _ xs = generatedError 14 xs


generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction26 ps0 vs
generatedState15 _ _ _ xs = generatedError 15 xs


generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState16 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction25 ps0 vs
generatedState16 _ _ _ xs = generatedError 16 xs


generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState17 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction14 ps0 vs
generatedState17 _ _ _ xs = generatedError 17 xs


generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState18 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState18 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (18:ss) xs
generatedState18 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (18:ss) xs
generatedState18 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState104 ps0 (x:vs) (18:ss) xs
generatedState18 _ _ _ xs = generatedError 18 xs


generatedState19 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction41 ps0 vs
generatedState19 _ _ _ xs = generatedError 19 xs


generatedState20 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState21 ps (x:vs) (20:ss) xs
generatedState20 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState24 ps0 (x:vs) (20:ss) xs
generatedState20 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState26 ps0 (x:vs) (20:ss) xs
generatedState20 _ _ _ xs = generatedError 20 xs


generatedState21 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState22 ps (x:vs) (21:ss) xs
generatedState21 _ _ _ xs = generatedError 21 xs


generatedState22 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState23 ps (x:vs) (22:ss) xs
generatedState22 _ _ _ xs = generatedError 22 xs


generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState23 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction33 ps0 vs
generatedState23 _ _ _ xs = generatedError 23 xs


generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState21 ps (x:vs) (24:ss) xs
generatedState24 ps0 vs ss (x@(AbsSynResult19 _ _):xs) = generatedState24 ps0 (x:vs) (24:ss) xs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction18 ps0 vs
generatedState24 ps0 vs ss (x@(AbsSynResult9 _ _):xs) = generatedState25 ps0 (x:vs) (24:ss) xs
generatedState24 _ _ _ xs = generatedError 24 xs


generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState25 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction17 ps0 vs
generatedState25 _ _ _ xs = generatedError 25 xs


generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = generatedState27 ps (x:vs) (26:ss) xs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState26 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = generatedState26 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState26 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState26 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState26 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction19 ps0 vs
generatedState26 ps0 vs ss (x@(AbsSynResult20 _ _):xs) = generatedState30 ps0 (x:vs) (26:ss) xs
generatedState26 ps0 vs ss (x@(AbsSynResult10 _ _):xs) = generatedState32 ps0 (x:vs) (26:ss) xs
generatedState26 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState99 ps0 (x:vs) (26:ss) xs
generatedState26 _ _ _ xs = generatedError 26 xs


generatedState27 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState28 ps (x:vs) (27:ss) xs
generatedState27 _ _ _ xs = generatedError 27 xs


generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (28:ss) xs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (28:ss) xs
generatedState28 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (28:ss) xs
generatedState28 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState29 ps0 (x:vs) (28:ss) xs
generatedState28 _ _ _ xs = generatedError 28 xs


generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState29 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction34 ps0 vs
generatedState29 _ _ _ xs = generatedError 29 xs


generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "paramtype"))):xs) = generatedState27 ps (x:vs) (30:ss) xs
generatedState30 ps0 vs ss (x@(AbsSynResult20 _ _):xs) = generatedState30 ps0 (x:vs) (30:ss) xs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction21 ps0 vs
generatedState30 ps0 vs ss (x@(AbsSynResult11 _ _):xs) = generatedState31 ps0 (x:vs) (30:ss) xs
generatedState30 _ _ _ xs = generatedError 30 xs


generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState31 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction20 ps0 vs
generatedState31 _ _ _ xs = generatedError 31 xs


generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState32 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState32 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState32 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState32 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState32 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = generatedState33 ps (x:vs) (32:ss) xs
generatedState32 ps0 vs ss (x@(AbsSynResult21 _ _):xs) = generatedState37 ps0 (x:vs) (32:ss) xs
generatedState32 _ _ _ xs = generatedError 32 xs


generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState34 ps (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState35 ps0 (x:vs) (33:ss) xs
generatedState33 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState36 ps0 (x:vs) (33:ss) xs
generatedState33 _ _ _ xs = generatedError 33 xs


generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState34 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction30 ps0 vs
generatedState34 _ _ _ xs = generatedError 34 xs


generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState35 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction29 ps0 vs
generatedState35 _ _ _ xs = generatedError 35 xs


generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState36 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction35 ps0 vs
generatedState36 _ _ _ xs = generatedError 36 xs


generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState37 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState37 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState37 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState37 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState38 ps (x:vs) (37:ss) xs
generatedState37 ps0 vs ss (x@(AbsSynResult22 _ _):xs) = generatedState40 ps0 (x:vs) (37:ss) xs
generatedState37 _ _ _ xs = generatedError 37 xs


generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState34 ps (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState35 ps0 (x:vs) (38:ss) xs
generatedState38 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState39 ps0 (x:vs) (38:ss) xs
generatedState38 _ _ _ xs = generatedError 38 xs


generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState39 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction37 ps0 vs
generatedState39 _ _ _ xs = generatedError 39 xs


generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState40 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState40 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState41 ps (x:vs) (40:ss) xs
generatedState40 ps0 vs ss (x@(AbsSynResult23 _ _):xs) = generatedState43 ps0 (x:vs) (40:ss) xs
generatedState40 _ _ _ xs = generatedError 40 xs


generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState34 ps (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState35 ps0 (x:vs) (41:ss) xs
generatedState41 ps0 vs ss (x@(AbsSynResult17 _ _):xs) = generatedState42 ps0 (x:vs) (41:ss) xs
generatedState41 _ _ _ xs = generatedError 41 xs


generatedState42 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction39 ps0 vs
generatedState42 _ _ _ xs = generatedError 42 xs


generatedState43 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState44 ps (x:vs) (43:ss) xs
generatedState43 ps0 vs ss (x@(AbsSynResult28 _ _):xs) = generatedState46 ps0 (x:vs) (43:ss) xs
generatedState43 ps0 vs ss (x@(AbsSynResult25 _ _):xs) = generatedState98 ps0 (x:vs) (43:ss) xs
generatedState43 _ _ _ xs = generatedError 43 xs


generatedState44 ps0 vs ss (x@(AbsSynToken (Token ps (TokenUpperIdentifier _))):xs) = generatedState45 ps (x:vs) (44:ss) xs
generatedState44 _ _ _ xs = generatedError 44 xs


generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState45 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction48 ps0 vs
generatedState45 _ _ _ xs = generatedError 45 xs


generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState44 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState47 ps (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynResult28 _ _):xs) = generatedState93 ps0 (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynResult29 _ _):xs) = generatedState94 ps0 (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynResult27 _ _):xs) = generatedState95 ps0 (x:vs) (46:ss) xs
generatedState46 ps0 vs ss (x@(AbsSynResult26 _ _):xs) = generatedState97 ps0 (x:vs) (46:ss) xs
generatedState46 _ _ _ xs = generatedError 46 xs


generatedState47 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState48 ps (x:vs) (47:ss) xs
generatedState47 _ _ _ xs = generatedError 47 xs


generatedState48 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState49 ps (x:vs) (48:ss) xs
generatedState48 _ _ _ xs = generatedError 48 xs


generatedState49 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState50 ps (x:vs) (49:ss) xs
generatedState49 _ _ _ xs = generatedError 49 xs


generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState50 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState50 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = generatedState51 ps (x:vs) (50:ss) xs
generatedState50 ps0 vs ss (x@(AbsSynResult35 _ _):xs) = generatedState53 ps0 (x:vs) (50:ss) xs
generatedState50 _ _ _ xs = generatedError 50 xs


generatedState51 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (51:ss) xs
generatedState51 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (51:ss) xs
generatedState51 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (51:ss) xs
generatedState51 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState52 ps0 (x:vs) (51:ss) xs
generatedState51 _ _ _ xs = generatedError 51 xs


generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction61 ps0 vs
generatedState52 _ _ _ xs = generatedError 52 xs


generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState53 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState53 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState54 ps (x:vs) (53:ss) xs
generatedState53 ps0 vs ss (x@(AbsSynResult32 _ _):xs) = generatedState60 ps0 (x:vs) (53:ss) xs
generatedState53 _ _ _ xs = generatedError 53 xs


generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState55 ps (x:vs) (54:ss) xs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState56 ps (x:vs) (54:ss) xs
generatedState54 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState57 ps0 (x:vs) (54:ss) xs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState58 ps (x:vs) (54:ss) xs
generatedState54 ps0 vs ss (x@(AbsSynToken (Token ps (TokenStringLit _))):xs) = generatedState59 ps (x:vs) (54:ss) xs
generatedState54 _ _ _ xs = generatedError 54 xs


generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "to"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction32 ps0 vs
generatedState55 _ _ _ xs = generatedError 55 xs


generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "to"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction31 ps0 vs
generatedState56 _ _ _ xs = generatedError 56 xs


generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction55 ps0 vs
generatedState57 _ _ _ xs = generatedError 57 xs


generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction54 ps0 vs
generatedState58 _ _ _ xs = generatedError 58 xs


generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction53 ps0 vs
generatedState59 _ _ _ xs = generatedError 59 xs


generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState60 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction58 ps0 vs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState60 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction58 ps0 vs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState60 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction58 ps0 vs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState60 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction58 ps0 vs
generatedState60 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState61 ps (x:vs) (60:ss) xs
generatedState60 ps0 vs ss (x@(AbsSynResult33 _ _):xs) = generatedState87 ps0 (x:vs) (60:ss) xs
generatedState60 _ _ _ xs = generatedError 60 xs


generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState62 ps0 (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynResult38 _ _):xs) = generatedState73 ps0 (x:vs) (61:ss) xs
generatedState61 ps0 vs ss (x@(AbsSynResult34 _ _):xs) = generatedState75 ps0 (x:vs) (61:ss) xs
generatedState61 _ _ _ xs = generatedError 61 xs


generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState62 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction62 ps0 vs
generatedState62 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = generatedState51 ps (x:vs) (62:ss) xs
generatedState62 ps0 vs ss (x@(AbsSynResult35 _ _):xs) = generatedState63 ps0 (x:vs) (62:ss) xs
generatedState62 _ _ _ xs = generatedError 62 xs


generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "^->"))):xs) = generatedState64 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "*->"))):xs) = generatedState65 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "->"))):xs) = generatedState66 ps (x:vs) (63:ss) xs
generatedState63 ps0 vs ss (x@(AbsSynResult37 _ _):xs) = generatedState67 ps0 (x:vs) (63:ss) xs
generatedState63 _ _ _ xs = generatedError 63 xs


generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction67 ps0 vs
generatedState64 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction67 ps0 vs
generatedState64 _ _ _ xs = generatedError 64 xs


generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 ps0 vs
generatedState65 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction66 ps0 vs
generatedState65 _ _ _ xs = generatedError 65 xs


generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction65 ps0 vs
generatedState66 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction65 ps0 vs
generatedState66 _ _ _ xs = generatedError 66 xs


generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState55 ps (x:vs) (67:ss) xs
generatedState67 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState56 ps (x:vs) (67:ss) xs
generatedState67 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState68 ps0 (x:vs) (67:ss) xs
generatedState67 _ _ _ xs = generatedError 67 xs


generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState68 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction64 ps0 vs
generatedState68 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "~"))):xs) = generatedState69 ps (x:vs) (68:ss) xs
generatedState68 ps0 vs ss (x@(AbsSynResult36 _ _):xs) = generatedState71 ps0 (x:vs) (68:ss) xs
generatedState68 _ _ _ xs = generatedError 68 xs


generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState55 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState56 ps (x:vs) (69:ss) xs
generatedState69 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState70 ps0 (x:vs) (69:ss) xs
generatedState69 _ _ _ xs = generatedError 69 xs


generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction63 ps0 vs
generatedState70 _ _ _ xs = generatedError 70 xs


generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "evaluating"))):xs) = generatedState71 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction56 ps0 vs
generatedState71 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "@"))):xs) = generatedState54 ps (x:vs) (71:ss) xs
generatedState71 ps0 vs ss (x@(AbsSynResult32 _ _):xs) = generatedState72 ps0 (x:vs) (71:ss) xs
generatedState71 _ _ _ xs = generatedError 71 xs


generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 5)) ps (drop 6 vs) (drop 6 ss) (x':x:xs)
  where x' = generatedReduction68 ps0 vs
generatedState72 _ _ _ xs = generatedError 72 xs


generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState13 ps (x:vs) (73:ss) xs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState14 ps (x:vs) (73:ss) xs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState15 ps (x:vs) (73:ss) xs
generatedState73 ps0 vs ss (x@(AbsSynResult16 _ _):xs) = generatedState62 ps0 (x:vs) (73:ss) xs
generatedState73 ps0 vs ss (x@(AbsSynResult38 _ _):xs) = generatedState73 ps0 (x:vs) (73:ss) xs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction60 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction60 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction60 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction60 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction60 ps0 vs
generatedState73 ps0 vs ss (x@(AbsSynResult34 _ _):xs) = generatedState74 ps0 (x:vs) (73:ss) xs
generatedState73 _ _ _ xs = generatedError 73 xs


generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction59 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction59 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction59 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction59 ps0 vs
generatedState74 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction59 ps0 vs
generatedState74 _ _ _ xs = generatedError 74 xs


generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState75 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction70 ps0 vs
generatedState75 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "restricting"))):xs) = generatedState76 ps (x:vs) (75:ss) xs
generatedState75 ps0 vs ss (x@(AbsSynResult39 _ _):xs) = generatedState86 ps0 (x:vs) (75:ss) xs
generatedState75 _ _ _ xs = generatedError 75 xs


generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState55 ps (x:vs) (76:ss) xs
generatedState76 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState56 ps (x:vs) (76:ss) xs
generatedState76 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState77 ps0 (x:vs) (76:ss) xs
generatedState76 ps0 vs ss (x@(AbsSynResult41 _ _):xs) = generatedState83 ps0 (x:vs) (76:ss) xs
generatedState76 ps0 vs ss (x@(AbsSynResult40 _ _):xs) = generatedState85 ps0 (x:vs) (76:ss) xs
generatedState76 _ _ _ xs = generatedError 76 xs


generatedState77 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "to"))):xs) = generatedState78 ps (x:vs) (77:ss) xs
generatedState77 _ _ _ xs = generatedError 77 xs


generatedState78 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState79 ps (x:vs) (78:ss) xs
generatedState78 ps0 vs ss (x@(AbsSynResult42 _ _):xs) = generatedState82 ps0 (x:vs) (78:ss) xs
generatedState78 _ _ _ xs = generatedError 78 xs


generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction75 ps0 vs
generatedState79 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator ","))):xs) = generatedState80 ps (x:vs) (79:ss) xs
generatedState79 _ _ _ xs = generatedError 79 xs


generatedState80 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState79 ps (x:vs) (80:ss) xs
generatedState80 ps0 vs ss (x@(AbsSynResult42 _ _):xs) = generatedState81 ps0 (x:vs) (80:ss) xs
generatedState80 _ _ _ xs = generatedError 80 xs


generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState81 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction74 ps0 vs
generatedState81 _ _ _ xs = generatedError 81 xs


generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState82 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction73 ps0 vs
generatedState82 _ _ _ xs = generatedError 82 xs


generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "IdentifierPrime" _))):xs) = generatedState55 ps (x:vs) (83:ss) xs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState56 ps (x:vs) (83:ss) xs
generatedState83 ps0 vs ss (x@(AbsSynResult18 _ _):xs) = generatedState77 ps0 (x:vs) (83:ss) xs
generatedState83 ps0 vs ss (x@(AbsSynResult41 _ _):xs) = generatedState83 ps0 (x:vs) (83:ss) xs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState83 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction72 ps0 vs
generatedState83 ps0 vs ss (x@(AbsSynResult40 _ _):xs) = generatedState84 ps0 (x:vs) (83:ss) xs
generatedState83 _ _ _ xs = generatedError 83 xs


generatedState84 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState84 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState84 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState84 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction71 ps0 vs
generatedState84 _ _ _ xs = generatedError 84 xs


generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction69 ps0 vs
generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction69 ps0 vs
generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction69 ps0 vs
generatedState85 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction69 ps0 vs
generatedState85 _ _ _ xs = generatedError 85 xs


generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction57 ps0 vs
generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction57 ps0 vs
generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction57 ps0 vs
generatedState86 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction57 ps0 vs
generatedState86 _ _ _ xs = generatedError 86 xs


generatedState87 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "where"))):xs) = generatedState88 ps (x:vs) (87:ss) xs
generatedState87 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState87 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState87 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState87 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState87 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState87 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction51 ps0 vs
generatedState87 ps0 vs ss (x@(AbsSynResult43 _ _):xs) = generatedState90 ps0 (x:vs) (87:ss) xs
generatedState87 ps0 vs ss (x@(AbsSynResult30 _ _):xs) = generatedState91 ps0 (x:vs) (87:ss) xs
generatedState87 ps0 vs ss (x@(AbsSynResult31 _ _):xs) = generatedState92 ps0 (x:vs) (87:ss) xs
generatedState87 _ _ _ xs = generatedError 87 xs


generatedState88 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "CodeBlock" _))):xs) = generatedState89 ps (x:vs) (88:ss) xs
generatedState88 _ _ _ xs = generatedError 88 xs


generatedState89 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState89 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState89 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction76 ps0 vs
generatedState89 _ _ _ xs = generatedError 89 xs


generatedState90 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction52 ps0 vs
generatedState90 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction52 ps0 vs
generatedState90 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction52 ps0 vs
generatedState90 _ _ _ xs = generatedError 90 xs


generatedState91 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 7)) ps (drop 8 vs) (drop 8 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState91 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 7)) ps (drop 8 vs) (drop 8 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState91 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 7)) ps (drop 8 vs) (drop 8 ss) (x':x:xs)
  where x' = generatedReduction50 ps0 vs
generatedState91 _ _ _ xs = generatedError 91 xs


generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 7)) ps (drop 8 vs) (drop 8 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 7)) ps (drop 8 vs) (drop 8 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState92 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 7)) ps (drop 8 vs) (drop 8 ss) (x':x:xs)
  where x' = generatedReduction49 ps0 vs
generatedState92 _ _ _ xs = generatedError 92 xs


generatedState93 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState93 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState93 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction47 ps0 vs
generatedState93 _ _ _ xs = generatedError 93 xs


generatedState94 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState94 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState94 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction46 ps0 vs
generatedState94 _ _ _ xs = generatedError 94 xs


generatedState95 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState44 ps (x:vs) (95:ss) xs
generatedState95 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "case"))):xs) = generatedState47 ps (x:vs) (95:ss) xs
generatedState95 ps0 vs ss (x@(AbsSynResult28 _ _):xs) = generatedState93 ps0 (x:vs) (95:ss) xs
generatedState95 ps0 vs ss (x@(AbsSynResult29 _ _):xs) = generatedState94 ps0 (x:vs) (95:ss) xs
generatedState95 ps0 vs ss (x@(AbsSynResult27 _ _):xs) = generatedState95 ps0 (x:vs) (95:ss) xs
generatedState95 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction45 ps0 vs
generatedState95 ps0 vs ss (x@(AbsSynResult26 _ _):xs) = generatedState96 ps0 (x:vs) (95:ss) xs
generatedState95 _ _ _ xs = generatedError 95 xs


generatedState96 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction44 ps0 vs
generatedState96 _ _ _ xs = generatedError 96 xs


generatedState97 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction43 ps0 vs
generatedState97 _ _ _ xs = generatedError 97 xs


generatedState98 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) ps (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction16 ps0 vs
generatedState98 _ _ _ xs = generatedError 98 xs


generatedState99 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState99 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState99 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState99 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState99 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState99 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState99 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState99 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction36 ps0 vs
generatedState99 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "stateextra"))):xs) = generatedState33 ps (x:vs) (99:ss) xs
generatedState99 ps0 vs ss (x@(AbsSynResult21 _ _):xs) = generatedState100 ps0 (x:vs) (99:ss) xs
generatedState99 _ _ _ xs = generatedError 99 xs


generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState100 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState100 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState100 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction38 ps0 vs
generatedState100 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "varextra"))):xs) = generatedState38 ps (x:vs) (100:ss) xs
generatedState100 ps0 vs ss (x@(AbsSynResult22 _ _):xs) = generatedState101 ps0 (x:vs) (100:ss) xs
generatedState100 _ _ _ xs = generatedError 100 xs


generatedState101 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState101 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction40 ps0 vs
generatedState101 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "standardenv"))):xs) = generatedState41 ps (x:vs) (101:ss) xs
generatedState101 ps0 vs ss (x@(AbsSynResult23 _ _):xs) = generatedState102 ps0 (x:vs) (101:ss) xs
generatedState101 _ _ _ xs = generatedError 101 xs


generatedState102 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "asttype"))):xs) = generatedState44 ps (x:vs) (102:ss) xs
generatedState102 ps0 vs ss (x@(AbsSynResult28 _ _):xs) = generatedState46 ps0 (x:vs) (102:ss) xs
generatedState102 ps0 vs ss (x@(AbsSynResult25 _ _):xs) = generatedState103 ps0 (x:vs) (102:ss) xs
generatedState102 _ _ _ xs = generatedError 102 xs


generatedState103 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 6)) ps (drop 7 vs) (drop 7 ss) (x':x:xs)
  where x' = generatedReduction15 ps0 vs
generatedState103 _ _ _ xs = generatedError 103 xs


generatedState104 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState104 _ _ _ xs = generatedError 104 xs


generatedState105 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState105 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState105 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (105:ss) xs
generatedState105 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (105:ss) xs
generatedState105 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState106 ps0 (x:vs) (105:ss) xs
generatedState105 _ _ _ xs = generatedError 105 xs


generatedState106 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState106 _ _ _ xs = generatedError 106 xs


generatedState107 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 ps (x:vs) (107:ss) xs
generatedState107 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState107 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState107 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState107 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState107 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState17 ps0 (x:vs) (107:ss) xs
generatedState107 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState108 ps0 (x:vs) (107:ss) xs
generatedState107 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState110 ps0 (x:vs) (107:ss) xs
generatedState107 _ _ _ xs = generatedError 107 xs


generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState108 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState108 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (108:ss) xs
generatedState108 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState109 ps0 (x:vs) (108:ss) xs
generatedState108 _ _ _ xs = generatedError 108 xs


generatedState109 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState109 _ _ _ xs = generatedError 109 xs


generatedState110 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState110 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState110 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (110:ss) xs
generatedState110 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (110:ss) xs
generatedState110 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState111 ps0 (x:vs) (110:ss) xs
generatedState110 _ _ _ xs = generatedError 110 xs


generatedState111 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState111 _ _ _ xs = generatedError 111 xs


generatedState112 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "precode"))):xs) = generatedState8 ps (x:vs) (112:ss) xs
generatedState112 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState112 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState112 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState112 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState112 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState112 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState112 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState112 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction11 ps0 vs
generatedState112 ps0 vs ss (x@(AbsSynResult14 _ _):xs) = generatedState10 ps0 (x:vs) (112:ss) xs
generatedState112 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState113 ps0 (x:vs) (112:ss) xs
generatedState112 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState118 ps0 (x:vs) (112:ss) xs
generatedState112 _ _ _ xs = generatedError 112 xs


generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 ps (x:vs) (113:ss) xs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState113 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState113 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState17 ps0 (x:vs) (113:ss) xs
generatedState113 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState114 ps0 (x:vs) (113:ss) xs
generatedState113 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState116 ps0 (x:vs) (113:ss) xs
generatedState113 _ _ _ xs = generatedError 113 xs


generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState114 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState114 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (114:ss) xs
generatedState114 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState115 ps0 (x:vs) (114:ss) xs
generatedState114 _ _ _ xs = generatedError 114 xs


generatedState115 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState115 _ _ _ xs = generatedError 115 xs


generatedState116 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState116 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState116 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (116:ss) xs
generatedState116 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (116:ss) xs
generatedState116 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState117 ps0 (x:vs) (116:ss) xs
generatedState116 _ _ _ xs = generatedError 116 xs


generatedState117 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState117 _ _ _ xs = generatedError 117 xs


generatedState118 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "outputprecode"))):xs) = generatedState12 ps (x:vs) (118:ss) xs
generatedState118 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState118 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState118 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState118 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction13 ps0 vs
generatedState118 ps0 vs ss (x@(AbsSynResult15 _ _):xs) = generatedState17 ps0 (x:vs) (118:ss) xs
generatedState118 ps0 vs ss (x@(AbsSynResult6 _ _):xs) = generatedState119 ps0 (x:vs) (118:ss) xs
generatedState118 ps0 vs ss (x@(AbsSynResult7 _ _):xs) = generatedState121 ps0 (x:vs) (118:ss) xs
generatedState118 _ _ _ xs = generatedError 118 xs


generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState119 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState119 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (119:ss) xs
generatedState119 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState120 ps0 (x:vs) (119:ss) xs
generatedState119 _ _ _ xs = generatedError 119 xs


generatedState120 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction2 ps0 vs
generatedState120 _ _ _ xs = generatedError 120 xs


generatedState121 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "basetype"))):xs) = generatedState121 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction42 ps0 vs
generatedState121 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCustom "Directive" "hasincludes"))):xs) = generatedState19 ps (x:vs) (121:ss) xs
generatedState121 ps0 vs ss (x@(AbsSynResult24 _ _):xs) = generatedState20 ps0 (x:vs) (121:ss) xs
generatedState121 ps0 vs ss (x@(AbsSynResult8 _ _):xs) = generatedState122 ps0 (x:vs) (121:ss) xs
generatedState121 _ _ _ xs = generatedError 121 xs


generatedState122 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 4)) ps (drop 5 vs) (drop 5 ss) (x':x:xs)
  where x' = generatedReduction1 ps0 vs
generatedState122 _ _ _ xs = generatedError 122 xs


generatedState123 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState123 _ _ _ xs = generatedError 123 xs


generatedReduction1 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult7 v4 ps4):(AbsSynResult5 v3 ps3):(AbsSynResult3 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction2 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult6 v4 ps4):(AbsSynResult5 v3 ps3):(AbsSynResult3 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction3 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult7 v4 ps4):(AbsSynResult4 v3 ps3):(AbsSynResult3 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction4 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult6 v4 ps4):(AbsSynResult4 v3 ps3):(AbsSynResult3 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction5 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult7 v4 ps4):(AbsSynResult5 v3 ps3):(AbsSynResult2 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction6 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult6 v4 ps4):(AbsSynResult5 v3 ps3):(AbsSynResult2 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction7 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult7 v4 ps4):(AbsSynResult4 v3 ps3):(AbsSynResult2 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction8 ps0 ((AbsSynResult8 v5 ps5):(AbsSynResult6 v4 ps4):(AbsSynResult4 v3 ps3):(AbsSynResult2 v2 ps2):(AbsSynResult12 v1 ps1):_) = AbsSynResult1 ((v1, v2, v3, v4, v5)) ps1

generatedReduction9 ps0 (_) = AbsSynResult2 (empty) ps0

generatedReduction10 ps0 ((AbsSynResult13 v1 ps1):_) = AbsSynResult3 (Just v1) ps1

generatedReduction11 ps0 (_) = AbsSynResult4 (empty) ps0

generatedReduction12 ps0 ((AbsSynResult14 v1 ps1):_) = AbsSynResult5 (Just v1) ps1

generatedReduction13 ps0 (_) = AbsSynResult6 (empty) ps0

generatedReduction14 ps0 ((AbsSynResult15 v1 ps1):_) = AbsSynResult7 (Just v1) ps1

generatedReduction15 ps0 ((AbsSynResult25 v7 ps7):(AbsSynResult23 v6 ps6):(AbsSynResult22 v5 ps5):(AbsSynResult21 v4 ps4):(AbsSynResult11 v3 ps3):(AbsSynResult9 v2 ps2):(AbsSynResult24 v1 ps1):_) = AbsSynResult8 (uncurry (SemanticsDef (fromList v2) (fromList v3) v4 v5 v6 v1) $ handleStatements v7) ps1

generatedReduction16 ps0 ((AbsSynResult25 v7 ps7):(AbsSynResult23 v6 ps6):(AbsSynResult22 v5 ps5):(AbsSynResult21 v4 ps4):(AbsSynResult10 v3 ps3):(AbsSynResult9 v2 ps2):(AbsSynResult24 v1 ps1):_) = AbsSynResult8 (uncurry (SemanticsDef (fromList v2) (fromList v3) v4 v5 v6 v1) $ handleStatements v7) ps1

generatedReduction17 ps0 ((AbsSynResult9 v2 ps2):(AbsSynResult19 v1 ps1):_) = AbsSynResult9 (v1:v2) ps1

generatedReduction18 ps0 ((AbsSynResult19 v1 ps1):_) = AbsSynResult9 ([v1]) ps1

generatedReduction19 ps0 (_) = AbsSynResult10 (empty) ps0

generatedReduction20 ps0 ((AbsSynResult11 v2 ps2):(AbsSynResult20 v1 ps1):_) = AbsSynResult11 (v1:v2) ps1

generatedReduction21 ps0 ((AbsSynResult20 v1 ps1):_) = AbsSynResult11 ([v1]) ps1

generatedReduction22 ps0 ((AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult12 (v2) ps1

generatedReduction23 ps0 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult13 (v2) ps1

generatedReduction24 ps0 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult14 (v2) ps1

generatedReduction25 ps0 ((AbsSynResult16 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult15 (v2) ps1

generatedReduction26 ps0 ((AbsSynToken (Token ps1 (TokenCustom "CodeBlock" v1))):_) = AbsSynResult16 (v1) ps1

generatedReduction27 ps0 ((AbsSynToken (Token ps1 (TokenCustom "IdentifierPrime" v1))):_) = AbsSynResult16 (v1) ps1

generatedReduction28 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult16 (v1) ps1

generatedReduction29 ps0 ((AbsSynResult16 v1 ps1):_) = AbsSynResult17 (v1) ps1

generatedReduction30 ps0 ((AbsSynToken (Token ps1 (TokenUpperIdentifier v1))):_) = AbsSynResult17 (v1) ps1

generatedReduction31 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult18 (v1) ps1

generatedReduction32 ps0 ((AbsSynToken (Token ps1 (TokenCustom "IdentifierPrime" v1))):_) = AbsSynResult18 (v1) ps1

generatedReduction33 ps0 ((AbsSynToken (Token ps3 (TokenStringLit v3))):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult19 ((v2, v3)) ps1

generatedReduction34 ps0 ((AbsSynResult16 v3 ps3):(AbsSynToken (Token ps2 (TokenIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult20 ((v2, v3)) ps1

generatedReduction35 ps0 ((AbsSynResult17 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult21 (v2) ps1

generatedReduction36 ps0 (_) = AbsSynResult21 ("()") ps0

generatedReduction37 ps0 ((AbsSynResult17 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult22 (v2) ps1

generatedReduction38 ps0 (_) = AbsSynResult22 ("()") ps0

generatedReduction39 ps0 ((AbsSynResult17 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult23 (v2) ps1

generatedReduction40 ps0 (_) = AbsSynResult23 ("return ()") ps0

generatedReduction41 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult24 (True) ps1

generatedReduction42 ps0 (_) = AbsSynResult24 (False) ps0

generatedReduction43 ps0 ((AbsSynResult26 v2 ps2):(AbsSynResult28 v1 ps1):_) = AbsSynResult25 ((Right v1):v2) ps1

generatedReduction44 ps0 ((AbsSynResult26 v2 ps2):(AbsSynResult27 v1 ps1):_) = AbsSynResult26 (v1:v2) ps1

generatedReduction45 ps0 ((AbsSynResult27 v1 ps1):_) = AbsSynResult26 ([v1]) ps1

generatedReduction46 ps0 ((AbsSynResult29 v1 ps1):_) = AbsSynResult27 (Left v1) ps1

generatedReduction47 ps0 ((AbsSynResult28 v1 ps1):_) = AbsSynResult27 (Right v1) ps1

generatedReduction48 ps0 ((AbsSynToken (Token ps2 (TokenUpperIdentifier v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult28 (v2) ps1

generatedReduction49 ps0 ((AbsSynResult31 v8 ps8):(AbsSynResult33 v7 ps7):(AbsSynResult32 v6 ps6):(AbsSynResult35 v5 ps5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult29 (uncurry (SemanticsRule v2 v4 v5 v6 "" v8) v7) ps1

generatedReduction50 ps0 ((AbsSynResult30 v8 ps8):(AbsSynResult33 v7 ps7):(AbsSynResult32 v6 ps6):(AbsSynResult35 v5 ps5):(AbsSynToken (Token ps4 (TokenCustom "CodeBlock" v4))):(AbsSynToken (Token ps3 v3)):(AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult29 (uncurry (SemanticsRule v2 v4 v5 v6 "" v8) v7) ps1

generatedReduction51 ps0 (_) = AbsSynResult30 (empty) ps0

generatedReduction52 ps0 ((AbsSynResult43 v1 ps1):_) = AbsSynResult31 (Just v1) ps1

generatedReduction53 ps0 ((AbsSynToken (Token ps2 (TokenStringLit v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult32 (SemanticsStaticBaseType v2) ps1

generatedReduction54 ps0 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult32 (SemanticsStaticType v2) ps1

generatedReduction55 ps0 ((AbsSynResult18 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult32 (SemanticsVarType v2) ps1

generatedReduction56 ps0 (_) = AbsSynResult32 (SemanticsCommandType) ps0

generatedReduction57 ps0 ((AbsSynResult39 v3 ps3):(AbsSynResult34 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult33 ((v2, v3)) ps1

generatedReduction58 ps0 (_) = AbsSynResult33 (([], [])) ps0

generatedReduction59 ps0 ((AbsSynResult34 v2 ps2):(AbsSynResult38 v1 ps1):_) = AbsSynResult34 (v1:v2) ps1

generatedReduction60 ps0 ((AbsSynResult38 v1 ps1):_) = AbsSynResult34 ([v1]) ps1

generatedReduction61 ps0 ((AbsSynResult16 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult35 (v2) ps1

generatedReduction62 ps0 (_) = AbsSynResult35 ("env") ps0

generatedReduction63 ps0 ((AbsSynResult18 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult36 (v2) ps1

generatedReduction64 ps0 (_) = AbsSynResult36 ("_") ps0

generatedReduction65 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult37 (SemanticsDepSingle) ps1

generatedReduction66 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult37 (SemanticsDepFold False) ps1

generatedReduction67 ps0 ((AbsSynToken (Token ps1 v1)):_) = AbsSynResult37 (SemanticsDepFold True) ps1

generatedReduction68 ps0 ((AbsSynResult32 v6 ps6):(AbsSynResult36 v5 ps5):(AbsSynResult18 v4 ps4):(AbsSynResult37 v3 ps3):(AbsSynResult35 v2 ps2):(AbsSynResult16 v1 ps1):_) = AbsSynResult38 (SemanticsRuleDependency v1 v2 v4 v5 (RawSemanticsDepType v6) v3) ps1

generatedReduction69 ps0 ((AbsSynResult40 v2 ps2):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult39 (v2) ps1

generatedReduction70 ps0 (_) = AbsSynResult39 ([]) ps0

generatedReduction71 ps0 ((AbsSynResult40 v2 ps2):(AbsSynResult41 v1 ps1):_) = AbsSynResult40 (v1:v2) ps1

generatedReduction72 ps0 ((AbsSynResult41 v1 ps1):_) = AbsSynResult40 ([v1]) ps1

generatedReduction73 ps0 ((AbsSynResult42 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynResult18 v1 ps1):_) = AbsSynResult41 (SemanticsTypeRestriction v1 v3) ps1

generatedReduction74 ps0 ((AbsSynResult42 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult42 (v1:v3) ps1

generatedReduction75 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult42 ([v1]) ps1

generatedReduction76 ps0 ((AbsSynToken (Token ps2 (TokenCustom "CodeBlock" v2))):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult43 (v2) ps1

