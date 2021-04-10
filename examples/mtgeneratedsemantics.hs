module MtGeneratedSemantics (runSemantics) where

import MtGeneratedParser
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Lens
import ParserRequirements
import Data.HashMap.Strict
import Data.List
import Data.Hashable


isBaseType :: String -> Bool
isBaseType t = elem t ["int","boolean"]
baseTypesC :: HashMap String String
baseTypesC = fromList [("int","int"),("boolean","int")]
paramTypesC :: HashMap String ([String] -> String)
paramTypesC = fromList []

type StateExtra = ()
type VarExtra = (Bool, Bool)
data PersistentState = PersistentState{ _nameCounter :: Int }
data VarType = BaseType String
             | FuncType [VarType] VarType
             | ParamType String [VarType]
             | CommandType deriving Eq
instance Show VarType where
    show (BaseType s) = s
    show (ParamType t vs) = (paramTypesC ! t) $ fmap show vs
    show (FuncType is o) = "function"
    show CommandType = "command"
instance Hashable VarType where
    hashWithSalt salt (BaseType s) = hashWithSalt salt s
    hashWithSalt salt (ParamType t vs) = hashWithSalt salt (t, vs)
    hashWithSalt salt (FuncType is o) = hashWithSalt salt (is, o)
    hashWithSalt salt CommandType = hashWithSalt salt "command"
data Var e = Var{ _varName :: String
                , _varScopeLevel :: Int
                , _varType :: VarType
                , _varCName :: String
                , _varExtra :: e
                } deriving Show
data VolatileState = VolatileState{ _vars :: HashMap String [Var VarExtra]
                                  , _currentScope :: Int
                                  , _staticFuncs :: HashMap (String, [VarType]) [Var ()]
                                  , _stateExtra :: StateExtra
                                  } deriving Show
data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState }
type StateResult a = StateT SemanticsState Result a
defaultSemanticState = SemanticsState (PersistentState 0) $ VolatileState empty (-1) empty $ ()
makeLenses ''SemanticsState
makeLenses ''PersistentState
makeLenses ''VolatileState
makeLenses ''Var
makeLenses ''VarType


makeDefined :: Var VarExtra -> VolatileState -> VolatileState
makeDefined v = modifyVar v $ varExtra . _1 .~ True
preCode :: String
preCode = unlines [
    "#include <stdio.h>",
    "void printInt(int x) { printf(\"%d\\n\", x); }"
    ]
_defined :: Var VarExtra -> Bool
_defined v = fst (_varExtra v)
_const :: Var VarExtra -> Bool
_const v = snd (_varExtra v)
addOp :: String -> String -> String -> StateResult ()
addOp i o name = modEnv $ addStaticFunc name [BaseType i, BaseType i] (BaseType o) name
intOps :: [String]
intOps = ["^", "*", "/", "+", "-"]
intCompOps :: [String]
intCompOps = ["<", "<=", "==", "!=", ">=", ">"]
boolCompOps :: [String]
boolCompOps = ["==", "&&", "||"]
makeStandardEnv = do
    modEnv $ addStaticFunc "print" [BaseType "int"] (CommandType) "printInt"
    modEnv $ addStaticFunc "print" [BaseType "boolean"] (CommandType) "printInt"
    mapM (addOp "int" "int") intOps
    mapM (addOp "int" "boolean") intCompOps
    mapM (addOp "boolean" "boolean") boolCompOps

class SemanticsEvaluable a where
    eval :: a -> StateResult (String, VarType)

incrementCounter :: StateResult Int
incrementCounter = do
   n <- use $ persistentState . nameCounter
   modifying (persistentState . nameCounter) (+1)
   return n
require :: String -> Bool -> StateResult ()
require _ True = return ()
require errMsg False = lift $ Error errMsg

forceMaybe :: String -> Maybe a -> StateResult a
forceMaybe errMsg Nothing = lift $ Error errMsg
forceMaybe _ (Just a) = return a

evalFold :: (SemanticsEvaluable a) => Bool -> [a] -> StateResult ([String], [VarType])
evalFold _ [] = return ([], [])
evalFold forwardEnv (e:es) = do
    env <- gets _volatileState
    ((code, env'), t) <- runEval (eval e) env
    if forwardEnv then assign volatileState env' else return ()
    (codes, ts) <- evalFold forwardEnv es
    return (code:codes, t:ts)

runEval :: StateResult (a, b) -> VolatileState -> StateResult ((a, VolatileState), b)
runEval f newVEnv = do
    oldVEnv <- use volatileState
    assign volatileState newVEnv
    (c, t) <- f
    modifiedVEnv <- use volatileState
    assign volatileState oldVEnv
    return ((c, modifiedVEnv), t)

getNewName :: StateResult String
getNewName = do
    n <- incrementCounter
    return $ "var" ++ show n
indent :: String -> String
indent s = intercalate "\n" $ fmap ("    "++) $ lines s
increaseScope :: VolatileState -> VolatileState
increaseScope = over currentScope (+1)
decreaseScope :: VolatileState -> VolatileState
decreaseScope s = s { _currentScope = max 0 $ cScope - 1
                    , _vars = removeVars $ _vars s
                    , _staticFuncs = removeVars $ _staticFuncs s }
  where
    cScope = _currentScope s
    removeVars vs = fmap (\vs' -> if _varScopeLevel (head vs') == cScope then tail vs' else vs') vs
toCType :: VarType -> String
toCType CommandType = "void"
toCType (BaseType s) = baseTypesC ! s
toCType (FuncType is o) = toCType o ++ " (*)(" ++ intercalate ", " (fmap toCType is) ++ ")"
toCType (ParamType n ts) = (paramTypesC ! n) $ fmap toCType ts
getVar :: String -> VolatileState -> Maybe (Var VarExtra)
getVar name env = env ^? vars . at name . _Just . _head
getStaticFunc :: String -> [VarType] -> VolatileState -> Maybe (Var ())
getStaticFunc name args env = env ^? staticFuncs . at (name, args) . _Just . _head
getVarFunc :: String -> [VarType] -> VolatileState -> Maybe (Var VarExtra, [VarType], VarType)
getVarFunc name args env = do
    var <- getVar name env
    case _varType var of
        FuncType is o -> Just (var, is, o)
        otherwise -> Nothing
modifyVar :: Var VarExtra -> (Var VarExtra -> Var VarExtra) -> VolatileState -> VolatileState
modifyVar (Var name _ _ _ _) f = vars . at name . _Just . _head %~ f
addVar :: String -> VarExtra -> VarType -> VolatileState -> StateResult (Var VarExtra, VolatileState)
addVar name extra t env = do
    newName <- getNewName
    let newVar = Var name (env ^. currentScope) t newName extra
    return (newVar, over vars (insertWith (++) name [newVar]) env)
addStaticFunc :: String -> [VarType] -> VarType -> String -> VolatileState -> VolatileState
addStaticFunc name args ret cName env = over (staticFuncs) (insertWith (++) (name, args) [newVar]) env
    where newVar = Var name (env ^. currentScope) ret cName ()
addVarFunc :: String -> [VarType] -> VarType -> VarExtra -> String -> VolatileState -> VolatileState
addVarFunc name args ret extra cName env = over vars (insertWith (++) name [newVar]) env
    where newVar = Var name (env ^. currentScope) (FuncType args ret) cName extra
modEnv :: (VolatileState -> VolatileState) -> StateResult ()
modEnv f = modifying volatileState f

runSemantics :: (SemanticsEvaluable a) => a -> Result String
runSemantics input = do
    ((code, _), _) <- runStateT ((makeStandardEnv) >> (modEnv increaseScope) >> eval input) defaultSemanticState
    let wrappedCode = "int main() {\n" ++ indent code ++ "\n    return 0;\n}"
    return $ (preCode) ++ "\n\n" ++ wrappedCode


cBinOp :: String -> String -> String -> String
cBinOp x y op = "(" ++ x ++ ") " ++ op ++ " (" ++ y ++ ")"
cUnOp :: String -> String -> String
cUnOp x op = "(" ++ op ++ "(" ++ x ++ "))"
cInt :: Int -> String
cInt = show
cFloat :: Float -> String
cFloat = show
cBool :: Bool -> String
cBool b = if b then "1" else "0"
cVar :: Var a -> String
cVar = _varCName
cAssignVar :: Var a -> String -> String
cAssignVar v x = cVar v ++ " = " ++ x ++ ";"
cCall :: Var a -> [String] -> String
cCall v args = cCallExpr v args ++ ";"
cCallExpr :: Var a -> [String] -> String
cCallExpr v args = cVar v ++ "(" ++ intercalate ", " args ++ ")"
cBlock :: String -> String
cBlock str = "{\n" ++ indent str ++ "\n}"
cIf :: String -> String -> String -> String
cIf cond cmdT cmdF = "if(" ++ cond ++ ")" ++ cBlock cmdT ++ " else " ++ cBlock cmdF
cSeq :: [String] -> String
cSeq = intercalate "\n"
cPass :: String
cPass = ""
cCreateVar :: Var a -> Maybe String -> String
cCreateVar v Nothing = toCType (_varType v) ++ " " ++ cVar v ++ ";"
cCreateVar v (Just s) = toCType (_varType v) ++ " " ++ cVar v ++ " = " ++ s ++ ";"
cWhile :: String -> String -> String
cWhile cond cmd = "while(" ++ cond ++ ")" ++ cBlock cmd
cRawFor :: Var a -> String -> String -> String -> String -> String
cRawFor v init cond step cmd = "for(" ++ cCreateVar v (Just init) ++ "; " ++ cond ++ "; " ++ step ++ ")" ++ cBlock cmd
cSimpleFor :: Var a -> String -> String -> String -> String -> String
cSimpleFor v init limit step cmd = cBlock $ "int limit = " ++ limit ++ ";\n" ++
                                            cRawFor v init "limit" (cVar v ++ " += " ++ step) cmd


generateCodeASTExpr :: ASTExpr -> StateResult (String, VarType)

instance SemanticsEvaluable ASTExpr where
    eval = generateCodeASTExpr

generateCodeASTCommand :: ASTCommand -> StateResult (String, VarType)

instance SemanticsEvaluable ASTCommand where
    eval = generateCodeASTCommand

generateCodeASTDecl :: ASTDecl -> StateResult (String, VarType)

instance SemanticsEvaluable ASTDecl where
    eval = generateCodeASTDecl



generateCodeASTExpr (ASTExprBinOp op x1 x2) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x1, env)
    ((x1s, _), t1) <- runEval (eval depVal0) depEnv0
    let (depVal1, depEnv1) = (x2, env)
    ((x2s, _), t2) <- runEval (eval depVal1) depEnv1
    var <- forceMaybe ("Operator " ++ op ++ " does not exist for types " ++ show t1 ++ " and " ++ show t2) $ getStaticFunc op [t1, t2] env
    let rType = var ^. varType
    return (cBinOp x1s x2s (cVar var), rType)

generateCodeASTExpr (ASTExprInt x) = do
    env <- use volatileState
    return (cInt x, BaseType "int")

generateCodeASTExpr (ASTExprBool b) = do
    env <- use volatileState
    return (cBool b, BaseType "boolean")

generateCodeASTExpr (ASTExprVar name) = do
    env <- use volatileState
    var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    require ("Variable " ++ name ++ " defined but not initialised") $ _defined var
    return (cVar var, BaseType "int")

generateCodeASTExpr (ASTExprUnOp "!" x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "boolean") ++ ", got " ++ show depType0) $ depType0 == BaseType "boolean"
    return (cUnOp xs "!", BaseType "boolean")

generateCodeASTExpr (ASTExprUnOp "-" x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    return (cUnOp xs "-", BaseType "int")

generateCodeASTCommand (ASTAssign name x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    require "Cannot assign to a constant variable" $ not $ _const var
    let env' = if not $ _defined var then makeDefined var env else env
    let output = (cAssignVar var xs, env')
    assign volatileState $ snd output
    return (fst output, CommandType)

generateCodeASTCommand (ASTCall name args) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (args, env)
    ((argsS, _), types) <- runEval (evalFold False depVal0) depEnv0
    var <- forceMaybe ("Function " ++ name ++ " does not exist for args " ++ (intercalate ", " $ fmap show types)) $ getStaticFunc name types env
    return (cCall var argsS, CommandType)

generateCodeASTCommand (ASTIf cond tCmd fCmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cond, env)
    ((condS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "boolean") ++ ", got " ++ show depType0) $ depType0 == BaseType "boolean"
    let (depVal1, depEnv1) = (tCmd, env)
    ((tCmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ show (CommandType) ++ ", got " ++ show depType1) $ depType1 == CommandType
    let (depVal2, depEnv2) = (fCmd, env)
    ((fCmdS, _), depType2) <- runEval (eval depVal2) depEnv2
    require ("Expected " ++ show (CommandType) ++ ", got " ++ show depType2) $ depType2 == CommandType
    return (cIf condS tCmdS fCmdS, CommandType)

generateCodeASTCommand (ASTWhile cond cmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cond, env)
    ((condS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "boolean") ++ ", got " ++ show depType0) $ depType0 == BaseType "boolean"
    let (depVal1, depEnv1) = (cmd, env)
    ((cmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ show (CommandType) ++ ", got " ++ show depType1) $ depType1 == CommandType
    return (cWhile condS cmdS, CommandType)

generateCodeASTCommand (ASTLet decls cmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (decls, env)
    ((declsS, env'), depType0) <- runEval (evalFold True depVal0) depEnv0
    require ("Expected " ++ show (CommandType) ++ "'s, got " ++ show depType0) $ all (==CommandType) depType0
    let (depVal1, depEnv1) = (cmd, increaseScope env')
    ((cmdS, env''), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ show (CommandType) ++ ", got " ++ show depType1) $ depType1 == CommandType
    let output = (cBlock $ cSeq $ declsS ++ [cmdS], decreaseScope env'')
    assign volatileState $ snd output
    return (fst output, CommandType)

generateCodeASTCommand (ASTSeq cmds) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cmds, env)
    ((cmdsS, env'), depType0) <- runEval (evalFold True depVal0) depEnv0
    require ("Expected " ++ show (CommandType) ++ "'s, got " ++ show depType0) $ all (==CommandType) depType0
    let output = (cSeq cmdsS, env')
    assign volatileState $ snd output
    return (fst output, CommandType)

generateCodeASTCommand (ASTPass) = do
    env <- use volatileState
    return (cPass, CommandType)

generateCodeASTDecl (ASTDeclConst name val) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (val, env)
    ((valS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    (var, env') <- addVar name (True, True) (BaseType "int") env
    let output = (cCreateVar var (Just valS), env')
    assign volatileState $ snd output
    return (fst output, CommandType)

generateCodeASTDecl (ASTDeclVar name Nothing) = do
    env <- use volatileState
    (var, env') <- addVar name (False, False) (BaseType "int") env
    let output = (cCreateVar var Nothing, env')
    assign volatileState $ snd output
    return (fst output, CommandType)

generateCodeASTDecl (ASTDeclVar name (Just val)) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (val, env)
    ((valS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    (var, env') <- addVar name (True, False) (BaseType "int") env
    let output = (cCreateVar var (Just valS), env')
    assign volatileState $ snd output
    return (fst output, CommandType)

