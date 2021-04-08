module CompilerGenerator where

import ParserGenerator
import System.IO
import System.Directory
import System.Environment
import System.FilePath.Posix
import Data.Char
import Semanticsparser
import Semantics
import SemanticsValidator
import SemanticsCodeGenerator
import MainCodeGenerator

-- add way to match param types on deps
--     maybe generalise the param type to have any amount of params ?
--     could use some kinda of NonEmpty array of VarTypes
-- remove the dumb %command stuff, its yuck
--     just use a new constructor
-- add something to functions to define includes they require, put a Set of includes in the persistent state



-- presets for code gen, user shouldnt need to know C
--     perhaps with some kind of ShowC class, with instances for a bunch of the basic types
--     then also some for like ops, blocks, loops, etc. or something idk
--     assignment will be a lot nicer with the typeToC funcs
-- error source poses would be nice
-- something for includes?

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
    (ext, preCode, outPreCode, semantics) <- runParser smt

    validatedSemantics <- validateSemantics semantics

    let code = generateSemanticsCode name parserName preCode outPreCode validatedSemantics

    return (code, ext)