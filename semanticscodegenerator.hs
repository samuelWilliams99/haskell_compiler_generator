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

generateSemanticsCode :: String -> String -> Maybe String -> Maybe String -> SemanticsDef -> String
generateSemanticsCode name parserName preCode outPreCode
    (SemanticsDef baseTypes paramTypes stateExtra varExtra stateDef rules astTypes) =
    intercalate "\n\n"
        [ generateModuleCode name
        , generateImportsCode parserName
        , generateTypeCode baseTypes paramTypes
        , generateStateTypeCode stateExtra varExtra
        , unindent $ fromMaybe "" preCode
        , classCode
        , utilsCode
        , generateEntryPoint outPreCode stateDef
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
    "import Data.List\n" ++
    "import Data.Hashable"

generateStateTypeCode :: (String, String) -> String -> String
generateStateTypeCode (st, sd) vt =
    "type StateExtra = " ++ trim st ++ "\n" ++
    "type VarExtra = " ++ trim vt ++ "\n" ++
    "data PersistentState = PersistentState{ _nameCounter :: Int }\n" ++
    "data VarType = BaseType String\n" ++
    "             | FuncType [VarType] VarType\n" ++
    "             | ParamType String VarType deriving Eq\n" ++
    "instance Show VarType where\n" ++
    "    show (BaseType s) = s\n" ++
    "    show (ParamType t v) = (paramTypesC ! t) $ show v\n" ++
    "    show (FuncType is o) = \"function\"\n" ++
    "instance Hashable VarType where\n" ++
    "    hashWithSalt salt (BaseType s) = hashWithSalt salt s\n" ++
    "    hashWithSalt salt (ParamType t v) = hashWithSalt salt (t, v)\n" ++
    "    hashWithSalt salt (FuncType is o) = hashWithSalt salt (is, o)\n" ++
    "data Var e = Var{ _varName :: String\n" ++
    "                , _varScopeLevel :: Int\n" ++
    "                , _varType :: VarType\n" ++
    "                , _varCName :: String\n" ++
    "                , _varExtra :: e\n" ++
    "                } deriving Show\n" ++
    "data VolatileState = VolatileState{ _vars :: HashMap String [Var VarExtra]\n" ++
    "                                  , _currentScope :: Int\n" ++
    "                                  , _staticFuncs :: HashMap (String, [VarType]) [Var ()]\n" ++
    "                                  , _stateExtra :: StateExtra\n" ++
    "                                  } deriving Show\n" ++
    "data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState }\n" ++
    "type StateResult a = StateT SemanticsState Result a\n" ++
    "defaultSemanticState = SemanticsState (PersistentState 0) $ VolatileState empty (-1) empty $ " ++ trim sd ++ "\n" ++
    "makeLenses ''SemanticsState\n" ++
    "makeLenses ''PersistentState\n" ++
    "makeLenses ''VolatileState\n" ++
    "makeLenses ''Var\n" ++
    "makeLenses ''VarType\n"

classCode :: String
classCode = "class SemanticsEvaluable a where\n" ++
            "    eval :: a -> StateResult (String, VarType)"

utilsCode :: String
utilsCode = "incrementCounter :: StateResult Int\n" ++
            "incrementCounter = do\n" ++
            "   n <- use $ persistentState . nameCounter\n"  ++
            "   modifying (persistentState . nameCounter) (+1)\n" ++
            "   return n\n" ++
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
            "    if forwardEnv then assign volatileState env' else return ()\n" ++
            "    (codes, ts) <- evalFold forwardEnv es\n" ++
            "    return (code:codes, t:ts)\n\n" ++
            "runEval :: StateResult (a, b) -> VolatileState -> StateResult ((a, VolatileState), b)\n" ++
            "runEval f newVEnv = do\n" ++
            "    oldVEnv <- use volatileState\n" ++
            "    assign volatileState newVEnv\n" ++
            "    (c, t) <- f\n" ++
            "    modifiedVEnv <- use volatileState\n" ++
            "    assign volatileState oldVEnv\n" ++
            "    return ((c, modifiedVEnv), t)\n\n" ++
            "getNewName :: StateResult String\n" ++
            "getNewName = do\n" ++
            "    n <- incrementCounter\n" ++
            "    return $ \"var\" ++ show n\n" ++
            "indent :: String -> String\n" ++
            "indent s = intercalate \"\\n\" $ fmap (\"    \"++) $ lines s\n" ++
            "increaseScope :: VolatileState -> VolatileState\n" ++
            "increaseScope = over currentScope (+1)\n" ++
            "decreaseScope :: VolatileState -> VolatileState\n" ++
            "decreaseScope s = s { _currentScope = max 0 $ cScope - 1\n" ++
            "                    , _vars = removeVars $ _vars s\n" ++
            "                    , _staticFuncs = removeVars $ _staticFuncs s }\n" ++
            "  where\n" ++
            "    cScope = _currentScope s\n" ++
            "    removeVars vs = fmap (\\vs' -> if _varScopeLevel (head vs') == cScope then tail vs' else vs') vs\n" ++
            "toCType :: VarType -> String\n" ++
            "toCType (BaseType \"%command\") = \"void\"\n" ++
            "toCType (BaseType s) = baseTypesC ! s\n" ++
            "toCType (FuncType is o) = toCType o ++ \" (*)(\" ++ intercalate \", \" (fmap toCType is) ++ \")\"\n" ++
            "toCType (ParamType n t) = (paramTypesC ! n) $ toCType t\n" ++
            "getVar :: String -> VolatileState -> Maybe (Var VarExtra)\n" ++
            "getVar name env = env ^? vars . at name . _Just . _head\n" ++
            "getStaticFunc :: String -> [VarType] -> VolatileState -> Maybe (Var ())\n" ++
            "getStaticFunc name args env = env ^? staticFuncs . at (name, args) . _Just . _head\n" ++
            "getVarFunc :: String -> [VarType] -> VolatileState -> Maybe (Var VarExtra, [VarType], VarType)\n" ++
            "getVarFunc name args env = do\n" ++
            "    var <- getVar name env\n" ++
            "    case _varType var of\n" ++
            "        FuncType is o -> Just (var, is, o)\n" ++
            "        otherwise -> Nothing\n" ++
            "modifyVar :: Var VarExtra -> (Var VarExtra -> Var VarExtra) -> VolatileState -> VolatileState\n" ++
            "modifyVar (Var name _ _ _ _) f = vars . at name . _Just . _head %~ f\n" ++
            "addVar :: String -> VarExtra -> VarType -> VolatileState -> StateResult (Var VarExtra, VolatileState)\n" ++
            "addVar name extra t env = do\n" ++
            "    newName <- getNewName\n" ++
            "    let newVar = Var name (env ^. currentScope) t newName extra\n" ++
            "    return (newVar, over vars (insertWith (++) name [newVar]) env)\n" ++
            "addStaticFunc :: String -> [VarType] -> VarType -> String -> VolatileState -> VolatileState\n" ++
            "addStaticFunc name args ret cName env = over (staticFuncs) (insertWith (++) (name, args) [newVar]) env\n" ++
            "    where newVar = Var name (env ^. currentScope) ret cName ()\n" ++
            "addVarFunc :: String -> [VarType] -> VarType -> VarExtra -> String -> VolatileState -> VolatileState\n" ++
            "addVarFunc name args ret extra cName env = over vars (insertWith (++) name [newVar]) env\n" ++
            "    where newVar = Var name (env ^. currentScope) (FuncType args ret) cName extra\n" ++
            "modEnv :: (VolatileState -> VolatileState) -> StateResult ()\n" ++
            "modEnv f = modifying volatileState f"

generateEntryPoint :: Maybe String -> String -> String
generateEntryPoint outPreCode stateDef = unlines [
    "runSemantics :: (SemanticsEvaluable a) => a -> Result String",
    "runSemantics input = do",
    "    ((code, _), _) <- runStateT ((" ++ stateDef ++ ") >> (modEnv increaseScope) >> eval input) defaultSemanticState",
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