module CompilerGenerator where

import ParserGenerator
import System.IO
import System.Directory
import System.Environment
import System.FilePath.Posix
import Data.Char
import Data.Maybe
import Semanticsparser
import Semantics
import SemanticsValidator
import SemanticsCodeGenerator
import MainCodeGenerator

lowerStr :: String -> String
lowerStr = map toLower

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

exportsMap :: Maybe String -> Maybe String
exportsMap Nothing = Just "IncludeMap (..), IncludeMapType (..)"
exportsMap (Just e) = Just $ e ++ ", IncludeMap (..), IncludeMapType (..)"

generateCompiler :: String -> String -> String -> Result (String, String, String)
generateCompiler gmr smt modulePrefix = do
    let parserModule = modulePrefix ++ "Parser"
    let semanticsModule = modulePrefix ++ "Semantics"
    (semanticsCode, ext, hasIncludes) <- generateSemantics smt parserModule semanticsModule
    parserCode <- eitherToResult $ generateParser gmr parserModule $ if hasIncludes then exportsMap else id
    let mainCode = generateMainCode parserModule semanticsModule ext hasIncludes

    let parserCode' = if hasIncludes then parserCode ++ "\n\n" ++ includeMapsCode else parserCode

    return (parserCode', semanticsCode, mainCode)

generateSemantics :: String -> String -> String -> Result (String, String, Bool)
generateSemantics smt parserName name = do
    (ext, imports, preCode, outPreCode, semantics) <- runParser smt

    validatedSemantics <- validateSemantics semantics

    let code = generateSemanticsCode name parserName imports preCode outPreCode validatedSemantics

    return (code, ext, _semanticsHasIncludes semantics)