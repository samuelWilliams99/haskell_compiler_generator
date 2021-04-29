module SimpleParser (runParser, module ParserRequirements, SimpleCommands (..), SimpleCommand (..), SimpleExpression (..)) where

import ParserRequirements
import Control.Applicative
data SimpleCommands = SimpleCommands [SimpleCommand] ParseState
data SimpleCommand = SimpleAssign String SimpleExpression ParseState
                   | SimplePrint SimpleExpression ParseState
data SimpleExpression = SimpleInt Int ParseState
                      | SimpleVar String ParseState

gScanner = Scanner{ separateCasedIdentifiers=False
                  , ignoreWhitespace=True
                  , ignoreComments=True
                  , operators=["="]
                  , keywords=["print"]
                  , lineComment=Nothing
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

generatedStates = [generatedState0, generatedState1, generatedState2, generatedState3, generatedState4, generatedState5, generatedState6, generatedState7, generatedState8, generatedState9, generatedState10, generatedState11, generatedState12, generatedState13, generatedState14]

data AbsSynToken t1 t2 t3 t4 t5 = AbsSynToken Token | AbsSynResult1 t1 ParseState | AbsSynResult2 t2 ParseState | AbsSynResult3 t3 ParseState | AbsSynResult4 t4 ParseState | AbsSynResult5 t5 ParseState

generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "print"))):xs) = generatedState1 ps (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState7 ps (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = generatedState0 ps (drop 0 vs) (drop 0 ss) (x':x:xs)
  where x' = generatedReduction3 ps0 vs
generatedState0 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState10 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult2 _ _):xs) = generatedState12 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult3 _ _):xs) = generatedState13 ps0 (x:vs) (0:ss) xs
generatedState0 ps0 vs ss (x@(AbsSynResult1 _ _):xs) = generatedState14 ps0 (x:vs) (0:ss) xs
generatedState0 _ _ _ xs = generatedError 0 xs


generatedState1 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOpenParen))):xs) = generatedState2 ps (x:vs) (1:ss) xs
generatedState1 _ _ _ xs = generatedError 1 xs


generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState3 ps (x:vs) (2:ss) xs
generatedState2 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState4 ps (x:vs) (2:ss) xs
generatedState2 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState5 ps0 (x:vs) (2:ss) xs
generatedState2 _ _ _ xs = generatedError 2 xs


generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "print"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction9 ps0 vs
generatedState3 _ _ _ xs = generatedError 3 xs


generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState4 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "print"))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction8 ps0 vs
generatedState4 _ _ _ xs = generatedError 4 xs


generatedState5 ps0 vs ss (x@(AbsSynToken (Token ps (TokenCloseParen))):xs) = generatedState6 ps (x:vs) (5:ss) xs
generatedState5 _ _ _ xs = generatedError 5 xs


generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState6 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "print"))):xs) = (generatedStates !! (ss !! 3)) ps (drop 4 vs) (drop 4 ss) (x':x:xs)
  where x' = generatedReduction7 ps0 vs
generatedState6 _ _ _ xs = generatedError 6 xs


generatedState7 ps0 vs ss (x@(AbsSynToken (Token ps (TokenOperator "="))):xs) = generatedState8 ps (x:vs) (7:ss) xs
generatedState7 _ _ _ xs = generatedError 7 xs


generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState3 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIntLit _))):xs) = generatedState4 ps (x:vs) (8:ss) xs
generatedState8 ps0 vs ss (x@(AbsSynResult5 _ _):xs) = generatedState9 ps0 (x:vs) (8:ss) xs
generatedState8 _ _ _ xs = generatedError 8 xs


generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState9 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "print"))):xs) = (generatedStates !! (ss !! 2)) ps (drop 3 vs) (drop 3 ss) (x':x:xs)
  where x' = generatedReduction6 ps0 vs
generatedState9 _ _ _ xs = generatedError 9 xs


generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenKeyword "print"))):xs) = generatedState1 ps (x:vs) (10:ss) xs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenIdentifier _))):xs) = generatedState7 ps (x:vs) (10:ss) xs
generatedState10 ps0 vs ss (x@(AbsSynResult4 _ _):xs) = generatedState10 ps0 (x:vs) (10:ss) xs
generatedState10 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction5 ps0 vs
generatedState10 ps0 vs ss (x@(AbsSynResult3 _ _):xs) = generatedState11 ps0 (x:vs) (10:ss) xs
generatedState10 _ _ _ xs = generatedError 10 xs


generatedState11 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 1)) ps (drop 2 vs) (drop 2 ss) (x':x:xs)
  where x' = generatedReduction4 ps0 vs
generatedState11 _ _ _ xs = generatedError 11 xs


generatedState12 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction2 ps0 vs
generatedState12 _ _ _ xs = generatedError 12 xs


generatedState13 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = (generatedStates !! (ss !! 0)) ps (drop 1 vs) (drop 1 ss) (x':x:xs)
  where x' = generatedReduction1 ps0 vs
generatedState13 _ _ _ xs = generatedError 13 xs


generatedState14 ps0 vs ss (x@(AbsSynToken (Token ps (TokenEOF))):xs) = return $ unpackFinal $ head vs
generatedState14 _ _ _ xs = generatedError 14 xs


generatedReduction1 ps0 ((AbsSynResult3 v1 ps1):_) = AbsSynResult1 (SimpleCommands v1 ps1) ps1

generatedReduction2 ps0 ((AbsSynResult2 v1 ps1):_) = AbsSynResult1 (SimpleCommands v1 ps1) ps1

generatedReduction3 ps0 (_) = AbsSynResult2 (empty) ps0

generatedReduction4 ps0 ((AbsSynResult3 v2 ps2):(AbsSynResult4 v1 ps1):_) = AbsSynResult3 (v1:v2) ps1

generatedReduction5 ps0 ((AbsSynResult4 v1 ps1):_) = AbsSynResult3 ([v1]) ps1

generatedReduction6 ps0 ((AbsSynResult5 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult4 (SimpleAssign v1 v3 ps1) ps1

generatedReduction7 ps0 ((AbsSynToken (Token ps4 v4)):(AbsSynResult5 v3 ps3):(AbsSynToken (Token ps2 v2)):(AbsSynToken (Token ps1 v1)):_) = AbsSynResult4 (SimplePrint v3 ps1) ps1

generatedReduction8 ps0 ((AbsSynToken (Token ps1 (TokenIntLit v1))):_) = AbsSynResult5 (SimpleInt v1 ps1) ps1

generatedReduction9 ps0 ((AbsSynToken (Token ps1 (TokenIdentifier v1))):_) = AbsSynResult5 (SimpleVar v1 ps1) ps1

