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

-- includes:
--     add a flag to deciding if you want to support includes
--     add a way to specify a map on the env when including, so includes are now [(String, VolatileState -> VolatileState)], rather than [String]
--         allows stuff like "import (func1, func2) from File", or "import File hiding (func1)"
--         can have some nice presets for whitelisting, blacklisting, renaming, etc.
--     ensure errors specify what file they came from

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
                writeFile (replaceFileName path $ (lowerStr modulePrefix) ++ "generatedparser.hs") parser
                writeFile (replaceFileName path "parserrequirements.hs") parserRequirements
                writeFile (replaceFileName path $ (lowerStr modulePrefix) ++ "generatedsemantics.hs") semantics
                writeFile (replaceFileName path $ (lowerStr modulePrefix) ++ "generatedcompiler.hs") mainCode

generateCompiler :: String -> String -> String -> Result (String, String, String)
generateCompiler gmr smt modulePrefix = do
    let parserModule = modulePrefix ++ "GeneratedParser"
    let semanticsModule = modulePrefix ++ "GeneratedSemantics"
    parserCode <- eitherToResult $ generateParser gmr parserModule
    (semanticsCode, ext) <- generateSemantics smt parserModule semanticsModule
    let mainCode = generateMainCode parserModule semanticsModule ext

    return (parserCode, semanticsCode, mainCode)

generateSemantics :: String -> String -> String -> Result (String, String)
generateSemantics smt parserName name = do
    (ext, imports, preCode, outPreCode, semantics) <- runParser smt

    validatedSemantics <- validateSemantics semantics

    let code = generateSemanticsCode name parserName imports preCode outPreCode validatedSemantics

    return (code, ext)