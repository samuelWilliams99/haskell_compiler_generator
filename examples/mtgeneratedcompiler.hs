module Main where
import System.IO
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.Console.GetOpt
import System.Exit
import Data.HashMap.Strict hiding (filter)
import Data.List hiding (insert)
import Data.Ord
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import MtGeneratedParser
import MtGeneratedSemantics
data Flag = Ext | Info | Input String | Output String | Dep String deriving (Show, Eq)
options :: [OptDescr Flag]
options = [ Option ['e'] ["ext", "extension"] (NoArg Ext) "input extension for this compiler"
          , Option [] ["info"] (NoArg Info) "information about this compiler"
          , Option ['i', 'c'] ["input", "code"] (ReqArg Input "FILE") "input FILE"
          , Option ['o'] ["output", "out"] (ReqArg Output "FILE") "output FILE"
          ]
handleArgs :: [String] -> IO ([Flag])
handleArgs argv =
    case getOpt Permute options argv of
        (o@(_:_),[],[]) -> return o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."
getIOPaths :: [Flag] -> (String, Maybe String)
getIOPaths [] = ("", Nothing)
getIOPaths ((Input s):fs) = (s, snd $ getIOPaths fs)
getIOPaths ((Output s):fs) = (fst $ getIOPaths fs, Just s)
getIOPaths (_:fs) = getIOPaths fs
main :: IO ()
main = do
    flags <- getArgs >>= handleArgs
    if elem Ext flags then putStrLn "Files in the form name.mt are compiled to name.c"
    else if elem Info flags then putStrLn "This compiler was automatically generated by a compiler generator written by Samuel Williams"
    else (uncurry runCompiler $ getIOPaths flags)
resultToIO :: Result a -> IO a
resultToIO (Error e) = die e
resultToIO (Result a) = return a
runCompiler :: String -> Maybe String -> IO ()
runCompiler inp mOutp
    | takeExtension inp /= ".mt" = die "Invalid file extension, must be .mt"
    | otherwise = do
        let outp = fromMaybe (replaceExtension inp "c") mOutp
        exists <- doesFileExist inp
        if not exists then
            die $ "Could not find file \"" ++ inp ++ "\""
        else do
            content <- readFile inp
            code <- resultToIO $ compile content
            writeFile outp code
compile :: String -> Result String
compile s = do
    syntax <- runParser s
    defaultState <- rDefaultState
    (code, _) <- runSemantics defaultState syntax
    let wrappedCode = "int main() {\n" ++ indent code ++ "\n    return 0;\n}"
    return $ _outPreCode ++ wrappedCode
