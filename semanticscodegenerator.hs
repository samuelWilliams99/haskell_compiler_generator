module SemanticsCodeGenerator (generateSemanticsCode, reindent) where

import Semantics
import Data.List
import Data.Char
import Data.Maybe
import Data.HashMap.Strict hiding (map, filter)

reindent :: Int -> String -> String
reindent n str = intercalate "\n" $ map (\line -> (replicate n ' ') ++ (drop minIndent line)) strLines
  where
    minIndent = minimum $ map (length . fst . span isSpace) $ strLines
    strLines = filter ((>0) . length . trim) $ lines str

trim = dropWhileEnd isSpace . dropWhile isSpace

unindent :: String -> String
unindent = reindent 0

generateSemanticsCode :: String -> String -> Maybe String -> SemanticsDef -> String
generateSemanticsCode name parserName outPreCode
    (SemanticsDef preCode baseTypes paramTypes stateExtra varExtra rules astTypes) =
    intercalate "\n\n"
        [ generateModuleCode name
        , generateImportsCode parserName
        , generateTypeCode baseTypes paramTypes
        , generateStateTypeCode stateExtra varExtra
        , unindent preCode
        , classCode
        , utilsCode
        , generateEntryPoint outPreCode
        , generateInstanceCode astTypes
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
    "import ParserRequirements\n" ++
    "import Data.HashMap.Strict\n" ++
    "import Data.List\n"

generateStateTypeCode :: (String, String) -> (String, String) -> String
generateStateTypeCode (st, sd) (vt, _) =
    "type StateExtra = " ++ trim st ++ "\n" ++
    "type VarExtra = " ++ trim vt ++ "\n" ++
    "data PersistentState = PersistentState{ _nameCounter :: Int }\n" ++
    "data VarType = BaseType String\n" ++
    "             | FuncType [([VarType], VarType, String)]\n" ++
    "             | ParamType String VarType deriving Eq\n" ++
    "instance Show VarType where\n" ++
    "    show (BaseType s) = s\n" ++
    "    show (ParamType t v) = (paramTypesC ! t) $ show v\n" ++
    "    show (FuncType fs) = \"function\"\n" ++
    "data Var = Var{ _name :: String\n" ++
    "              , _scopeLevel :: Int\n" ++
    "              , _varType :: VarType\n" ++
    "              , _cName :: String\n" ++
    "              , _varExtra :: VarExtra\n" ++
    "              } deriving Show\n" ++
    "data VolatileState = VolatileState{ _vars :: HashMap String [Var]\n" ++
    "                                  , _currentScope :: Int\n" ++
    "                                  , _stateExtra :: StateExtra\n" ++
    "                                  } deriving Show\n" ++
    "data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState }\n" ++
    "type StateResult a = StateT SemanticsState Result a\n" ++
    "defaultSemanticState = SemanticsState (PersistentState 0) $ VolatileState empty 0 $ " ++ trim sd ++ "\n" ++
    "makeLenses ''SemanticsState\n" ++
    "makeLenses ''PersistentState\n" ++
    "makeLenses ''VolatileState\n" ++
    "makeLenses ''Var\n" ++
    "makeLenses ''VarType\n"

classCode :: String
classCode = "class SemanticsEvaluable a where\n" ++
            "    eval :: a -> StateResult (String, VarType)"

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
            "evalFold :: (SemanticsEvaluable a) => Bool -> [a] -> StateResult ([String], [VarType])\n" ++
            "evalFold _ [] = return ([], [])\n" ++
            "evalFold forwardEnv (e:es) = do\n" ++
            "    env <- gets _volatileState\n" ++
            "    ((code, env'), t) <- runEval (eval e) env\n" ++
            "    if forwardEnv then setVolatile env' else return ()\n" ++
            "    (codes, ts) <- evalFold forwardEnv es\n" ++
            "    return (code:codes, t:ts)\n\n" ++
            "runEval :: StateResult (a, b) -> VolatileState -> StateResult ((a, VolatileState), b)\n" ++
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
            "indent s = intercalate \"\\n\" $ fmap (\"    \"++) $ lines s\n" ++
            "increaseScope :: VolatileState -> VolatileState\n" ++
            "increaseScope s = s { _currentScope=(_currentScope s) + 1 }\n" ++
            "decreaseScope :: VolatileState -> VolatileState\n" ++
            "decreaseScope s = s { _currentScope = max 0 $ cScope - 1\n" ++
            "                    , _vars = fmap (\\vs -> if _scopeLevel (head vs) == cScope then tail vs else vs) $ _vars s }\n" ++
            "  where\n" ++
            "    cScope = _currentScope s\n" ++
            "toCType :: VarType -> String\n" ++
            "toCType (BaseType \"%command\") = \"void\"\n" ++
            "toCType (BaseType s) = baseTypesC ! s\n" ++
            "toCType (FuncType fs) = \"struct { \" ++ (concat $ fmap\n" ++
            "    (\\(is, o, i) -> toCType o ++ \" (*func\" ++ show i ++ \")(\" ++ intercalate \", \" (fmap toCType is) ++ \"); \") fs)\n" ++
            "    ++ \"}\"\n" ++
            "toCType (ParamType n t) = (paramTypesC ! n) $ toCType t\n" ++
            "getVar :: String -> VolatileState -> Maybe Var\n" ++
            "getVar name env = env ^? vars . at name . _Just . _head\n" ++
            "getFuncVar :: String -> [VarType] -> VolatileState -> Maybe (Var, ([VarType], VarType, String))\n" ++
            "getFuncVar name args env = getVar name env >>= (\\var -> case _varType var of\n" ++
            "                                              (FuncType fs) -> fmap (\\d -> (var, d)) $\n" ++
            "                                                               find (\\(args', _, _) -> args == args') fs\n" ++
            "                                              otherwise     -> Nothing)\n" ++
            "getFunc :: String -> [VarType] -> VolatileState -> Maybe (VarType, String)\n" ++
            "getFunc name args env = do\n" ++
            "                            (var, (_, ret, i)) <- getFuncVar name args env\n" ++
            "                            return (ret, _cName var ++ \"->func\" ++ i)\n" ++
            "getOp :: String -> [VarType] -> VolatileState -> Maybe (VarType, String)\n" ++
            "getOp name args env = do\n" ++
            "                          (_, (_, ret, i)) <- getFuncVar name args env\n" ++
            "                          return (ret, i)\n" ++
            "modifyVar :: Var -> (Var -> Var) -> VolatileState -> VolatileState\n" ++
            "modifyVar (Var name _ _ _ _) f = vars . at name . _Just . _head %~ f\n" ++
            "addVar :: String -> VarExtra -> VarType -> VolatileState -> StateResult (Var, VolatileState)\n" ++
            "addVar name extra t env = do\n" ++
            "    newName <- getNewName\n" ++
            "    let newVar = Var name (env ^. currentScope) t newName extra\n" ++
            "    return (newVar, over vars (insertWith (++) name [newVar]) env)\n"

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
generateInstanceCode (t:ts) = astFuncName t ++ " :: " ++ t ++ " -> StateResult (String, VarType)\n\n" ++
                              "instance SemanticsEvaluable " ++ t ++ " where\n" ++
                              "    eval = " ++ astFuncName t ++ "\n\n" ++
                              generateInstanceCode ts

generateTypeCode :: HashMap String String -> HashMap String String -> String
generateTypeCode baseTypes paramTypes = "isBaseType :: String -> Bool\n" ++
                                        "isBaseType t = elem t " ++ show (keys baseTypes) ++ "\n" ++
                                        "baseTypesC :: HashMap String String\n" ++
                                        "baseTypesC = " ++ show baseTypes ++ "\n" ++
                                        "paramTypesC :: HashMap String (String -> String)\n" ++
                                        "paramTypesC = fromList " ++ show (fmap (\(n, f) -> "(" ++ show n ++ ", " ++ f ++ ")") $ toList paramTypes)

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
generateDepTypeCode (BuiltSemanticsDepTypeCompare t) depTypeStr (SemanticsDepFold _) =
    "    require (\"Expected \" ++ show (" ++ show t ++ ") ++ \"'s, got \" ++ show " ++ depTypeStr ++ ") $ all (==" ++ show t ++ ") " ++ depTypeStr ++ "\n"
generateDepTypeCode (BuiltSemanticsDepTypeCompare t) depTypeStr _ =
    "    require (\"Expected \" ++ show (" ++ show t ++ ") ++ \", got \" ++ show " ++ depTypeStr ++ ") $ " ++ depTypeStr ++ " == " ++ show t ++ "\n"

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