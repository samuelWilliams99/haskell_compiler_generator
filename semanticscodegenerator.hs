module SemanticsCodeGenerator (generateSemanticsCode, reindent) where

import Semantics
import Data.List
import Data.Char
import Data.Maybe

reindent :: Int -> String -> String
reindent n str = intercalate "\n" $ map (\line -> (replicate n ' ') ++ (drop minIndent line)) strLines
  where
    minIndent = minimum $ map (length . fst . span isSpace) $ strLines
    strLines = filter ((>0) . length . trim) $ lines str

trim = dropWhileEnd isSpace . dropWhile isSpace

unindent :: String -> String
unindent = reindent 0

generateSemanticsCode :: String -> String -> Maybe String -> SemanticsDef -> String
generateSemanticsCode name parserName outPreCode (SemanticsDef preCode baseTypes (stateType, stateDef) rules astTypes) =
    intercalate "\n\n"
        [ generateModuleCode name
        , generateImportsCode parserName
        , unindent preCode
        , classCode
        , utilsCode
        , generateEntryPoint outPreCode
        , generateStateTypeCode stateType stateDef
        , generateInstanceCode astTypes
        , generateBaseTypeCode baseTypes
        , generateRulesCode rules
        ]

generateModuleCode :: String -> String
generateModuleCode name = "module " ++ name ++ " (runSemantics) where"

generateImportsCode :: String -> String
generateImportsCode parserName =
    "import " ++ parserName ++ "\n" ++
    "import Control.Monad.Trans.State.Lazy\n" ++
    "import Control.Monad.Trans.Class\n" ++
    "import Control.Lens\n" ++
    "import ParserRequirements"

generateStateTypeCode :: String -> String -> String
generateStateTypeCode t v =
    "data PersistentState = PersistentState{ _nameCounter :: Int }\n" ++
    "type VolatileState = " ++ t ++ "\n" ++
    "data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState }\n" ++
    "type StateResult a = StateT SemanticsState Result a\n" ++
    "defaultSemanticState = SemanticsState (PersistentState 0) $ " ++ v ++ "\n" ++
    "makeLenses ''SemanticsState\n" ++
    "makeLenses ''PersistentState"

classCode :: String
classCode = "class SemanticsEvaluable a where\n" ++
            "    eval :: a -> StateResult (String, String)"

utilsCode :: String
utilsCode = "setVolatile :: VolatileState -> StateResult ()\n" ++
            "setVolatile v = do\n" ++
            "    env <- get\n" ++
            "    put $ env { _volatileState=v }\n\n" ++
            "incrementCounter :: StateResult Int\n" ++
            "incrementCounter = do\n" ++
            "   env <- get\n" ++
            "   let pState = _persistentState env\n" ++
            "   let n = _nameCounter pState\n" ++
            "   put env { _persistentState=(pState { _nameCounter = (n+1) }) }\n" ++
            "   return n\n\n" ++
            "require :: String -> Bool -> StateResult ()\n" ++
            "require _ True = return ()\n" ++
            "require errMsg False = lift $ Error errMsg\n\n" ++
            "forceMaybe :: String -> Maybe a -> StateResult a\n" ++
            "forceMaybe errMsg Nothing = lift $ Error errMsg\n" ++
            "forceMaybe _ (Just a) = return a\n\n" ++
            "evalFold :: (SemanticsEvaluable a) => Bool -> [a] -> StateResult ([String], [String])\n" ++
            "evalFold _ [] = return ([], [])\n" ++
            "evalFold forwardEnv (e:es) = do\n" ++
            "    env <- gets _volatileState\n" ++
            "    ((code, env'), t) <- runEval (eval e) env\n" ++
            "    if forwardEnv then setVolatile env' else return ()\n" ++
            "    (codes, ts) <- evalFold forwardEnv es\n" ++
            "    return (code:codes, t:ts)\n\n" ++
            "runEval :: StateResult (a, a) -> VolatileState -> StateResult ((a, VolatileState), a)\n" ++
            "runEval f newVEnv = do\n" ++
            "    oldVEnv <- gets _volatileState\n" ++
            "    setVolatile newVEnv\n" ++
            "    (c, t) <- f\n" ++
            "    modifiedVEnv <- gets _volatileState\n" ++
            "    setVolatile oldVEnv\n" ++
            "    return ((c, modifiedVEnv), t)\n\n" ++
            "getNewName :: StateResult String\n" ++
            "getNewName = do\n" ++
            "    n <- incrementCounter\n" ++
            "    return $ \"var\" ++ show n\n" ++
            "indent :: String -> String\n" ++
            "indent s = intercalate \"\\n\" $ fmap (\"    \"++) $ lines s"

generateEntryPoint :: Maybe String -> String
generateEntryPoint outPreCode = unlines [
    "runSemantics :: (SemanticsEvaluable a) => a -> Result String",
    "runSemantics input = do",
    "    ((code, _), _) <- runStateT (eval input) defaultSemanticState",
    "    let wrappedCode = \"int main() {\\n\" ++ indent code ++ \"\\n    return 0;\\n}\"",
    (case outPreCode of
         Nothing -> "    return wrappedCode"
         Just s  -> "    return $ (" ++ s ++ ") ++ \"\\n\\n\" ++ wrappedCode"
    )
    ]

astFuncName :: String -> String
astFuncName t = "generateCode" ++ t

generateInstanceCode :: [String] -> String
generateInstanceCode [] = ""
generateInstanceCode (t:ts) = astFuncName t ++ " :: " ++ t ++ " -> StateResult (String, String)\n\n" ++
                              "instance SemanticsEvaluable " ++ t ++ " where\n" ++
                              "    eval = " ++ astFuncName t ++ "\n\n" ++
                              generateInstanceCode ts

generateBaseTypeCode :: [String] -> String
generateBaseTypeCode baseTypes = "isBaseType :: String -> Bool\n" ++
                                 "isBaseType t = elem t " ++ show baseTypes

getEvalFunc :: SemanticsRuleDependencyIterType -> String
getEvalFunc SemanticsDepSingle = "eval"
getEvalFunc (SemanticsDepFold True) = "evalFold True"
getEvalFunc (SemanticsDepFold False) = "evalFold False"

generateDepEvalCode :: SemanticsRuleDependencyIterType -> String -> String -> String -> Int -> String
generateDepEvalCode depType input output depTypeStr i =
    concat [ "    let (depVal", show i, ", depEnv", show i, ") = ", trim input, "\n"
           , "    (", trim output, ", ", depTypeStr, ")"
           , " <- runEval (", getEvalFunc depType, " depVal", show i, ") depEnv", show i
           ]

genererateDepType :: SemanticsDepOutputType -> Int -> String
genererateDepType (BuiltSemanticsDepTypeAssign name) _ = name
genererateDepType (BuiltSemanticsDepTypeCompare _) i = "depType" ++ show i

generateDepTypeCode :: SemanticsDepOutputType -> String -> SemanticsRuleDependencyIterType -> String
generateDepTypeCode (BuiltSemanticsDepTypeAssign _) _ _ = ""
generateDepTypeCode (BuiltSemanticsDepTypeCompare (SemanticsBaseType t)) depTypeStr (SemanticsDepFold _) =
    "    require (\"Expected \" ++ " ++ show t ++ " ++ \"'s, got \" ++ show " ++ depTypeStr ++ ") $ all (==" ++ show t ++ ") " ++ depTypeStr ++ "\n"
generateDepTypeCode (BuiltSemanticsDepTypeCompare t) depTypeStr _ =
    "    require (\"Expected \" ++ " ++ show t ++ " ++ \", got \" ++ " ++ depTypeStr ++ ") $ " ++ depTypeStr ++ " == " ++ show t ++ "\n"

generateDepsCode :: Int -> [SemanticsRuleDependency] -> String
generateDepsCode i [] = ""
generateDepsCode i ((SemanticsRuleDependency input output outputType usesEnv depType):ds) =
    evalCode ++ "\n" ++
    typeCode ++
    rest
  where
    inputStr = if usesEnv then input else "(" ++ input ++ ", env)"
    outputStr = if usesEnv then output else "(" ++ output ++ ", _)"
    depTypeStr = genererateDepType outputType i
    evalCode = generateDepEvalCode depType inputStr outputStr depTypeStr i
    typeCode = generateDepTypeCode outputType depTypeStr depType

    rest = generateDepsCode (i + 1) ds

generateRestrictionsCode :: [SemanticsTypeRestriction] -> String
generateRestrictionsCode [] = ""
generateRestrictionsCode ((SemanticsTypeRestriction name ts):rs) =
    "    require (\"Expected " ++ intercalate " or " ts ++ ", got \" ++ " ++ name ++ ") $ elem " ++ name ++ " $ " ++ show ts ++ "\n" ++ rest
  where
    rest = generateRestrictionsCode rs

generateRuleReturnCode :: Bool -> String -> SemanticsType -> String
generateRuleReturnCode True out t = "    let output = " ++ trim out ++ "\n" ++
                                    "    assign volatileState $ snd output\n" ++
                                    "    return (fst output, " ++ show t ++ ")"
generateRuleReturnCode False out t = "    return (" ++ trim out ++ ", " ++ show t ++ ")"

generateRulesCode :: [SemanticsRule] -> String
generateRulesCode [] = ""
generateRulesCode (r:rs) =
    concat [ funcName
           , " ("
           , pattern
           , ") = do\n"
           , "    env <- use volatileState\n"
           , depsCode
           , restrictCode
           , whereCode
           , ret, "\n\n"
           , rest
           ]
  where
    funcName = astFuncName $ _semanticsRuleAstType r
    pattern = trim $ _semanticsRulePattern r
    depsCode = generateDepsCode 0 $ _semanticsRuleDeps r
    restrictCode = generateRestrictionsCode $ _semanticsRuleTypeRestrictions r
    whereCode = fromMaybe "" $ fmap ((++"\n") . reindent 4) $ _semanticsRuleWhere r
    ret = generateRuleReturnCode (_semanticsRuleChangesEnv r) (_semanticsRuleOutput r) (_semanticsRuleType r)
    rest = generateRulesCode rs