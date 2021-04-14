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

generateSemanticsCode :: String -> String -> Maybe String -> Maybe String -> Maybe String -> SemanticsDef -> String
generateSemanticsCode name parserName imports preCode outPreCode
    (SemanticsDef baseTypes paramTypes stateExtra varExtra env _ rules astTypes) =
    intercalate "\n\n"
        [ generateModuleCode name
        , generateImportsCode parserName imports
        , generateTypeCode baseTypes paramTypes
        , generateStateTypeCode stateExtra varExtra env
        , unindent $ fromMaybe "" preCode
        , generateOutPreCode outPreCode
        , classCode
        , utilsCode
        , entryPoint
        , cPresetsCode
        , generateInstanceCode astTypes
        , generateRulesCode rules
        ]

generateModuleCode :: String -> String
generateModuleCode name = "module " ++ name ++
    " (runSemantics, rDefaultState, _outPreCode, indent, " ++
    "VolatileState (..), PersistentState (..), SemanticsState (..), Var (..)) where"

generateImportsCode :: String -> Maybe String -> String
generateImportsCode parserName mImports =
    "import " ++ parserName ++ "\n" ++
    "import Control.Monad.Trans.State.Lazy\n" ++
    "import Control.Monad.Trans.Class\n" ++
    "import Control.Lens\n" ++
    "import ParserRequirements\n" ++
    "import Data.HashMap.Strict\n" ++
    "import Data.List hiding (union)\n" ++
    "import Data.Hashable\n" ++
    "import Data.Maybe\n" ++
    "import Data.Monoid\n" ++
    unindent (fromMaybe "" mImports)

generateStateTypeCode :: String -> String -> String -> String
generateStateTypeCode st vt env =
    "type StateExtra = " ++ trim st ++ "\n" ++
    "type VarExtra = " ++ trim vt ++ "\n" ++
    "data PersistentState = PersistentState{ _nameCounter :: Int }\n" ++
    "data VarType = BaseType String\n" ++
    "             | FuncType [VarType] VarType\n" ++
    "             | ParamType String [VarType]\n" ++
    "             | CommandType deriving Eq\n" ++
    "instance Show VarType where\n" ++
    "    show (BaseType s) = s\n" ++
    "    show (ParamType t vs) = (paramTypesC ! t) $ fmap show vs\n" ++
    "    show (FuncType is o) = \"function\"\n" ++
    "    show CommandType = \"command\"\n" ++
    "instance Hashable VarType where\n" ++
    "    hashWithSalt salt (BaseType s) = hashWithSalt salt s\n" ++
    "    hashWithSalt salt (ParamType t vs) = hashWithSalt salt (t, vs)\n" ++
    "    hashWithSalt salt (FuncType is o) = hashWithSalt salt (is, o)\n" ++
    "    hashWithSalt salt CommandType = hashWithSalt salt \"command\"\n" ++
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
    "instance Semigroup VolatileState where\n" ++
    "    (VolatileState vs1 _ fs1 e1) <> (VolatileState vs2 _ fs2 e2) = VolatileState (union vs2 vs1) 0 (union fs2 fs1) (e1 <> e2)\n" ++
    "instance Monoid VolatileState where\n" ++
    "    mempty = VolatileState empty (-1) empty mempty\n" ++
    "data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState, _psState :: ParseState }\n" ++
    "type StateResult a = StateT SemanticsState Result a\n" ++
    "defaultPersistentState = PersistentState 0\n" ++
    "makeLenses ''SemanticsState\n" ++
    "makeLenses ''PersistentState\n" ++
    "makeLenses ''VolatileState\n" ++
    "makeLenses ''Var\n" ++
    "makeLenses ''VarType\n" ++
    "rDefaultState :: Result SemanticsState\n" ++
    "rDefaultState = fmap snd $ runStateT (" ++ env ++ " >> modEnv increaseScope) (SemanticsState defaultPersistentState mempty $ parseState \"\")\n"

classCode :: String
classCode = "class SemanticsEvaluable a where\n" ++
            "    eval :: a -> StateResult (String, VarType)"

generateOutPreCode :: Maybe String -> String
generateOutPreCode mp = "_outPreCode :: String\n" ++
                        "_outPreCode = " ++ case mp of
                            Just p -> p ++ " ++ \"\\n\\n\"\n"
                            Nothing -> "\"\"\n"

utilsCode :: String
utilsCode = "incrementCounter :: StateResult Int\n" ++
            "incrementCounter = do\n" ++
            "   n <- use $ persistentState . nameCounter\n"  ++
            "   modifying (persistentState . nameCounter) (+1)\n" ++
            "   return n\n" ++
            "err :: String -> StateResult a\n" ++
            "err errMsg = use psState >>= \\ps -> lift $ Error $ errMsg ++ \" at \" ++ showPos ps\n\n" ++
            "require :: String -> Bool -> StateResult ()\n" ++
            "require _ True = return ()\n" ++
            "require errMsg False = err errMsg\n\n" ++
            "forceMaybe :: String -> Maybe a -> StateResult a\n" ++
            "forceMaybe errMsg Nothing = err errMsg\n" ++
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
            "toCType CommandType = \"void\"\n" ++
            "toCType (BaseType s) = baseTypesC ! s\n" ++
            "toCType (FuncType is o) = toCType o ++ \" (*)(\" ++ intercalate \", \" (fmap toCType is) ++ \")\"\n" ++
            "toCType (ParamType n ts) = (paramTypesC ! n) $ fmap toCType ts\n" ++
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

entryPoint :: String
entryPoint = unlines [
    "runSemantics :: (SemanticsEvaluable a) => SemanticsState -> a -> Result (String, SemanticsState)",
    "runSemantics inpState input = do",
    "    ((code, _), s) <- runStateT (eval input) inpState",
    "    return (code, s)"
    ]

cPresetsCode :: String
cPresetsCode = "cBinOp :: String -> String -> String -> String\n" ++
               "cBinOp x y op = \"(\" ++ x ++ \") \" ++ op ++ \" (\" ++ y ++ \")\"\n" ++
               "cUnOp :: String -> String -> String\n" ++
               "cUnOp x op = \"(\" ++ op ++ \"(\" ++ x ++ \"))\"\n" ++
               "cInt :: Int -> String\n" ++
               "cInt = show\n" ++
               "cFloat :: Float -> String\n" ++
               "cFloat = show\n" ++
               "cBool :: Bool -> String\n" ++
               "cBool b = if b then \"1\" else \"0\"\n" ++
               "cVar :: Var a -> String\n" ++
               "cVar = _varCName\n" ++
               "cAssignVar :: Var a -> String -> String\n" ++
               "cAssignVar v x = cVar v ++ \" = \" ++ x ++ \";\"\n" ++
               "cCall :: Var a -> [String] -> String\n" ++
               "cCall v args = cCallExpr v args ++ \";\"\n" ++
               "cCallExpr :: Var a -> [String] -> String\n" ++
               "cCallExpr v args = cVar v ++ \"(\" ++ intercalate \", \" args ++ \")\"\n" ++
               "cBlock :: String -> String\n" ++
               "cBlock str = \"{\\n\" ++ indent str ++ \"\\n}\"\n" ++
               "cIf :: [(String, String)] -> Maybe String -> String\n" ++
               "cIf cs mElse = (concat $ fmap (\\(cond, cmd) -> \"if(\" ++ cond ++ \")\" ++ cBlock cmd ++ \" else \") cs) ++ cBlock (fromMaybe \"\" mElse)\n" ++
               "cSimpleIf :: String -> String -> String -> String\n" ++
               "cSimpleIf cond t f = cIf [(cond, t)] (Just f)\n" ++
               "cSeq :: [String] -> String\n" ++
               "cSeq = intercalate \"\\n\"\n" ++
               "cPass :: String\n" ++
               "cPass = \"\"\n" ++
               "cCreateVar :: Var a -> Maybe String -> String\n" ++
               "cCreateVar v Nothing = toCType (_varType v) ++ \" \" ++ cVar v ++ \";\"\n" ++
               "cCreateVar v (Just s) = toCType (_varType v) ++ \" \" ++ cVar v ++ \" = \" ++ s ++ \";\"\n" ++
               "cWhile :: String -> String -> String\n" ++
               "cWhile cond cmd = \"while(\" ++ cond ++ \")\" ++ cBlock cmd\n" ++
               "cRawFor :: Var a -> String -> String -> String -> String -> String\n" ++
               "cRawFor v init cond step cmd = \"for(\" ++ cCreateVar v (Just init) ++ \"; \" ++ cond ++ \"; \" ++ step ++ \")\" ++ cBlock cmd\n" ++
               "cSimpleFor :: Var a -> String -> String -> String -> String -> String\n" ++
               "cSimpleFor v init limit step cmd = cBlock $ \"int limit = \" ++ limit ++ \";\\n\" ++\n" ++
               "                                            cRawFor v init \"limit\" (cVar v ++ \" += \" ++ step) cmd\n"

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
                                        "paramTypesC :: HashMap String ([String] -> String)\n" ++
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
genererateDepType (BuiltSemanticsDepTypeAssign name) _ = trim name
genererateDepType (BuiltSemanticsDepTypeCompare _) i = "depType" ++ show i

generateDepTypeCodePre :: SemanticsType -> String -> Bool -> String
generateDepTypeCodePre t depTypeStr plr =
    "    require (\"Expected \" ++ show (" ++ trim (show t) ++ ") ++ \"" ++
    (if plr then "'s" else "") ++
    ", got \" ++ show " ++ depTypeStr ++ ") $ "

generateDepTypeCode :: SemanticsDepOutputType -> String -> SemanticsRuleDependencyIterType -> String
generateDepTypeCode (BuiltSemanticsDepTypeAssign _) _ _ = ""
generateDepTypeCode (BuiltSemanticsDepTypeCompare t) depTypeStr (SemanticsDepFold _) =
    generateDepTypeCodePre t depTypeStr True ++ "all (==" ++ trim (show t) ++ ") " ++ depTypeStr ++ "\n"
generateDepTypeCode (BuiltSemanticsDepTypeCompare t) depTypeStr _ =
    generateDepTypeCodePre t depTypeStr False ++ depTypeStr ++ " == " ++ trim (show t) ++ "\n"

generateDepsCode :: Int -> [SemanticsRuleDependency] -> String
generateDepsCode i [] = ""
generateDepsCode i ((SemanticsRuleDependency input output outputType usesEnv depType):ds) =
    "    assign psState ps\n" ++
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
    "    require (\"Expected " ++ intercalate " or " ts ++ ", got \" ++ show " ++ name ++ ") $ " ++
    "elem " ++ name ++ " $ " ++ show (fmap SemanticsStaticBaseType ts) ++ "\n" ++ rest
  where
    rest = generateRestrictionsCode rs

generateRuleReturnCode :: Bool -> String -> SemanticsType -> String
generateRuleReturnCode True out t = "    let output = " ++ trim out ++ "\n" ++
                                    "    assign volatileState $ snd output\n" ++
                                    "    return (fst output, " ++ trim (show t) ++ ")"
generateRuleReturnCode False out t = "    return (" ++ trim out ++ ", " ++ trim (show t) ++ ")"

generateRulesCode :: [SemanticsRule] -> String
generateRulesCode [] = ""
generateRulesCode (r:rs) =
    concat [ funcName
           , " ("
           , pattern
           , ") = do\n"
           , "    env <- use volatileState\n"
           , depsCode
           , "    assign psState ps\n"
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