{-|
Module      : CompilerGenerator
Description : Generates Haskell Compilers from Syntactic and Semantic definitions, stored within .gmr and .smt files.
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

This module uses the ParserGenerator module, and as such, the .gmr file input for this module follows the same structure as that.
Similarly to the ParserGenerator module, this module can be invoked both within haskell in a pure computation, or by command line/via an IO computation.
The IO method of invoking the compiler generator will handle all input and output files for you, whereas using the haskell functions is a little more involved, as you are expected to correctly name the file and generate the requirements.
-}
module CompilerGenerator (runCompilerGenerator, generateCompiler, generateSemantics) where

import ParserGenerator
import System.IO
import System.Directory
import System.Environment
import System.FilePath.Posix
import Data.Char
import Data.Maybe
import Data.List
import Semanticsparser
import Semantics
import SemanticsValidator
import SemanticsCodeGenerator
import MainCodeGenerator

lowerStr :: String -> String
lowerStr = map toLower

main :: IO ()
main = do
    args <- getArgs
    runCompilerGenerator $ head args

-- | This function takes a path to file with no extension, and expects a .gmr and .smt file be present. For example, calling this function with @examples/mt@ will expect that @examples/mt.gmr@ and @examples/mt.smt@ exist.
-- The 4 files required for a compiler will be generated in the same location as the input files, named after the input files. For example, a compiler by the name @mt@ would generate the following files:
--
-- * @mtcompiler.hs@
--
-- * @mtparser.hs@
--
-- * @mtsemantics.hs@
--
-- * @parserrequirements.hs@
runCompilerGenerator :: String -> IO ()
runCompilerGenerator path = do
    let gmrPath = path ++ ".gmr"
    let smtPath = path ++ ".smt"
    gmrExists <- doesFileExist gmrPath
    smtExists <- doesFileExist smtPath
    if not gmrExists then
        putStrLn ("Could not find gmr file \"" ++ gmrPath ++ "\"")
    else if not smtExists then
        putStrLn ("Could not find smt file \"" ++ smtPath ++ "\"")
    else do
        gmrContent <- readFile gmrPath
        smtContent <- readFile smtPath

        let modulePrefix = pathToModule path

        case generateCompiler gmrContent smtContent modulePrefix  of
            Error e -> putStrLn e
            Result (parser, semantics, mainCode) -> do
                writeFile (replaceFileName path $ (lowerStr modulePrefix) ++ "parser.hs") parser
                writeFile (replaceFileName path "parserrequirements.hs") parserRequirements
                writeFile (replaceFileName path $ (lowerStr modulePrefix) ++ "semantics.hs") semantics
                writeFile (replaceFileName path $ (lowerStr modulePrefix) ++ "compiler.hs") mainCode

includeMapsCode :: String
includeMapsCode = unlines [
    "data IncludeMap = IncludeMap{ _includeMapType :: IncludeMapType",
    "                            , _nextIncludeMap :: Maybe IncludeMap } deriving Show",
    "data IncludeMapType = IncludeMapEverything | IncludeMapWhitelist [String] | IncludeMapBlacklist [String] | IncludeMapRename [(String, String)] deriving Show",
    "infixr 0 `andThen`",
    "andThen :: IncludeMap -> IncludeMap -> IncludeMap",
    "andThen a b = a { _nextIncludeMap = Just b }",
    "everything :: IncludeMap",
    "everything = IncludeMap IncludeMapEverything Nothing",
    "whitelist :: [String] -> IncludeMap",
    "whitelist xs = IncludeMap (IncludeMapWhitelist xs) Nothing",
    "blacklist :: [String] -> IncludeMap",
    "blacklist xs = IncludeMap (IncludeMapBlacklist xs) Nothing",
    "rename :: [(String, String)] -> IncludeMap",
    "rename ns = IncludeMap (IncludeMapRename ns) Nothing"
    ]

formaliseExports :: [String] -> String
formaliseExports es = intercalate ", " $ fmap (++( " (..)")) es

addExports :: [String] -> Maybe String -> Maybe String
addExports es Nothing = Just $ formaliseExports es
addExports es (Just e) = Just $ e ++ ", " ++ formaliseExports es

-- | Generates the code for the parser, semantics and compiler from the input definitions.
-- This function can fail for invalid inputs.
generateCompiler :: String -- ^ Contents of the .gmr file
                 -> String -- ^ Contents of the .smt file
                 -> String -- ^ Compiler name
                 -> Result (String, String, String) -- ^ Failure-capable tuple of @(parserCode, semanticsCode, compilerCode)@
generateCompiler gmr smt modulePrefix = do
    let parserModule = modulePrefix ++ "Parser"
    let semanticsModule = modulePrefix ++ "Semantics"
    (semanticsCode, ext, semantics) <- generateSemantics smt parserModule semanticsModule
    let hasIncludes = _semanticsHasIncludes semantics
    let asttypes = _semanticsAstTypes semantics
    let exports = asttypes ++ if hasIncludes then ["IncludeMap", "IncludeMapType"] else []

    parserCode <- eitherToResult $ generateParser gmr parserModule $ addExports exports
    let mainCode = generateMainCode parserModule semanticsModule ext hasIncludes

    let parserCode' = if hasIncludes then parserCode ++ "\n\n" ++ includeMapsCode else parserCode

    return (parserCode', semanticsCode, mainCode)

-- | Generates the semantics code from an .smt file, along with information needed to generate the main compiler file.
generateSemantics :: String -- ^ Contents of the .smt file
                  -> String -- ^ Module name for the parser
                  -> String -- ^ Compiler name
                  -> Result (String, String, SemanticsDef) -- ^ Failure-capable tuple of @(semanticsCode, fileExtension, semantics)@
generateSemantics smt parserName name = do
    (ext, imports, preCode, outPreCode, semantics) <- runParser smt

    validatedSemantics <- validateSemantics semantics

    let code = generateSemanticsCode name parserName imports preCode outPreCode validatedSemantics

    return (code, ext, semantics)