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

-- generalise the variable lookup and scope shit, most languages need this, the user shouldnt have to code it themselves
--     some increase and decrease scope functions that handle the scope value and remove vars that should be gone
--     some env changes surpass scope changes, such as defining an existing lower scope var
--     some languages separate vars and funcs, some dont, some way of generalising this concept
-- support parameterised types, like arrays, allow the user to choose the type of types, default is String
--     will need to change grammar to like this

-- make way to set standard env
--     probably some function to add functions lol
--     prob need to add a preCode thing to persistent env
--     then add the wacky fukin function def shit to it
--     idfk



-- error source poses would be nice
-- something for includes?
-- presets for code gen, user shouldnt need to know C
--     perhaps with some kind of ShowC class, with instances for a bunch of the basic types
--     then also some for like ops, blocks, loops, etc. or something idk

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