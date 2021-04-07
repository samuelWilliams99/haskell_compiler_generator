module MtGeneratedSemantics (runSemantics) where

import MtGeneratedParser
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Lens
import ParserRequirements
import Data.HashMap.Strict
import Data.List


isBaseType :: String -> Bool
isBaseType t = elem t ["int","boolean"]
baseTypesC :: HashMap String String
baseTypesC = fromList [("int","int"),("boolean","int")]
paramTypesC :: HashMap String (String -> String)
paramTypesC = fromList []

type StateExtra = ()
type VarExtra = (Bool, Bool)
data PersistentState = PersistentState{ _nameCounter :: Int }
data VarType = BaseType String
             | FuncType [([VarType], VarType, Int)]
             | ParamType String VarType deriving Eq
instance Show VarType where
    show (BaseType s) = s
    show (ParamType t v) = (paramTypesC ! t) $ show v
    show (FuncType fs) = "function"
data Var = Var{ _name :: String
              , _scopeLevel :: Int
              , _varType :: VarType
              , _cName :: String
              , _varExtra :: VarExtra
              } deriving Show
data VolatileState = VolatileState{ _vars :: HashMap String [Var]
                                  , _currentScope :: Int
                                  , _stateExtra :: StateExtra
                                  } deriving Show
data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState }
type StateResult a = StateT SemanticsState Result a
defaultSemanticState = SemanticsState (PersistentState 0) $ VolatileState empty 0 $ ()
makeLenses ''SemanticsState
makeLenses ''PersistentState
makeLenses ''VolatileState
makeLenses ''Var
makeLenses ''VarType


makeDefined :: Var -> VolatileState -> VolatileState
makeDefined v = modifyVar v $ varExtra . _1 .~ True
preCode :: String
preCode = unlines [
    "#include <stdio.h>",
    "void printInt(int x) { printf(\"%d\\n\", x); }"
    ]
_defined :: Var -> Bool
_defined v = fst (_varExtra v)
_const :: Var -> Bool
_const v = snd (_varExtra v)

class SemanticsEvaluable a where
    eval :: a -> StateResult (String, VarType)

setVolatile :: VolatileState -> StateResult ()
setVolatile v = do
    env <- get
    put $ env { _volatileState=v }

incrementCounter :: StateResult Int
incrementCounter = do
   env <- get
   let pState = _persistentState env
   let n = _nameCounter pState
   put env { _persistentState=(pState { _nameCounter = (n+1) }) }
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
    if forwardEnv then setVolatile env' else return ()
    (codes, ts) <- evalFold forwardEnv es
    return (code:codes, t:ts)

runEval :: StateResult (a, b) -> VolatileState -> StateResult ((a, VolatileState), b)
runEval f newVEnv = do
    oldVEnv <- gets _volatileState
    setVolatile newVEnv
    (c, t) <- f
    modifiedVEnv <- gets _volatileState
    setVolatile oldVEnv
    return ((c, modifiedVEnv), t)

getNewName :: StateResult String
getNewName = do
    n <- incrementCounter
    return $ "var" ++ show n
indent :: String -> String
indent s = intercalate "\n" $ fmap ("    "++) $ lines s
increaseScope :: VolatileState -> VolatileState
increaseScope s = s { _currentScope=(_currentScope s) + 1 }
decreaseScope :: VolatileState -> VolatileState
decreaseScope s = s { _currentScope = max 0 $ cScope - 1
                    , _vars = fmap (\vs -> if _scopeLevel (head vs) == cScope then tail vs else vs) $ _vars s }
  where
    cScope = _currentScope s
toCType :: VarType -> String
toCType (BaseType "%command") = "void"
toCType (BaseType s) = baseTypesC ! s
toCType (FuncType fs) = "struct { " ++ (concat $ fmap
    (\(is, o, i) -> toCType o ++ " (*func" ++ show i ++ ")(" ++ intercalate ", " (fmap toCType is) ++ "); ") fs)
    ++ "}"
toCType (ParamType n t) = (paramTypesC ! n) $ toCType t
getVar :: String -> VolatileState -> Maybe Var
getVar name env = env ^? vars . at name . _Just . _head
getFunc :: String -> [VarType] -> VolatileState -> Maybe (VarType, String)
getFunc name args env = getVar name env >>= (\var -> case _varType var of
                                                (FuncType fs) -> fmap (\(_, t, i) -> (t, _cName var ++ "->func" ++ show i)) $
                                                                 find (\(args', _, _) -> args == args') fs
                                                otherwise     -> Nothing)
modifyVar :: Var -> (Var -> Var) -> VolatileState -> VolatileState
modifyVar (Var name _ _ _ _) f = vars . at name . _Just . _head %~ f
addVar :: String -> VarExtra -> VarType -> VolatileState -> StateResult (Var, VolatileState)
addVar name extra t env = do
    newName <- getNewName
    let newVar = Var name (env ^. currentScope) t newName extra
    return (newVar, over vars (insertWith (++) name [newVar]) env)


runSemantics :: (SemanticsEvaluable a) => a -> Result String
runSemantics input = do
    ((code, _), _) <- runStateT (eval input) defaultSemanticState
    let wrappedCode = "int main() {\n" ++ indent code ++ "\n    return 0;\n}"
    return $ (preCode) ++ "\n\n" ++ wrappedCode


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
    (rType, fName) <- forceMaybe ("Operator " ++ op ++ " does not exist for types " ++ show t1 ++ " and " ++ show t2) $ getFunc op [t1, t2] env
    return ("(" ++ x1s ++ ") " ++ fName ++ " (" ++ x2s ++ ")", rType)

generateCodeASTExpr (ASTExprInt x) = do
    env <- use volatileState
    return (show x, BaseType "int")

generateCodeASTExpr (ASTExprBool b) = do
    env <- use volatileState
    return (if b then "1" else "0", BaseType "boolean")

generateCodeASTExpr (ASTExprVar name) = do
    env <- use volatileState
    var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    require ("Variable " ++ name ++ " defined but not initialised") $ _defined var
    return (_cName var, BaseType "int")

generateCodeASTExpr (ASTExprUnOp "!" x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "boolean") ++ ", got " ++ show depType0) $ depType0 == BaseType "boolean"
    return ("!(" ++ xs ++ ")", BaseType "boolean")

generateCodeASTExpr (ASTExprUnOp "-" x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    return ("-(" ++ xs ++ ")", BaseType "int")

generateCodeASTCommand (ASTAssign name x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    require "Cannot assign to a constant variable" $ not $ _const var
    let env' = if not $ _defined var then makeDefined var env else env
    let output = (_cName var ++ " = " ++ xs ++ ";", env')
    assign volatileState $ snd output
    return (fst output, BaseType "%command")

generateCodeASTCommand (ASTCall name args) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (args, env)
    ((argsS, _), types) <- runEval (evalFold False depVal0) depEnv0
    (_, fName) <- forceMaybe ("Function " ++ name ++ " does not exist for args " ++ (intercalate ", " $ fmap show types)) $ getFunc name types env
    return (fName ++ "(" ++ intercalate ", " argsS ++ ");", BaseType "%command")

generateCodeASTCommand (ASTIf cond tCmd fCmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cond, env)
    ((condS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "boolean") ++ ", got " ++ show depType0) $ depType0 == BaseType "boolean"
    let (depVal1, depEnv1) = (tCmd, env)
    ((tCmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ show (BaseType "%command") ++ ", got " ++ show depType1) $ depType1 == BaseType "%command"
    let (depVal2, depEnv2) = (fCmd, env)
    ((fCmdS, _), depType2) <- runEval (eval depVal2) depEnv2
    require ("Expected " ++ show (BaseType "%command") ++ ", got " ++ show depType2) $ depType2 == BaseType "%command"
    return ("if( " ++ condS ++ " ){\n" ++ indent tCmdS ++ "\n} else {\n" ++ indent fCmdS ++ "\n}", BaseType "%command")

generateCodeASTCommand (ASTWhile cond cmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cond, env)
    ((condS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "boolean") ++ ", got " ++ show depType0) $ depType0 == BaseType "boolean"
    let (depVal1, depEnv1) = (cmd, env)
    ((cmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ show (BaseType "%command") ++ ", got " ++ show depType1) $ depType1 == BaseType "%command"
    return ("while( " ++ condS ++ " ){\n" ++ indent cmdS ++ "\n}", BaseType "%command")

generateCodeASTCommand (ASTLet decls cmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (decls, env)
    ((declsS, env'), depType0) <- runEval (evalFold True depVal0) depEnv0
    require ("Expected " ++ show (BaseType "%command") ++ "'s, got " ++ show depType0) $ all (==BaseType "%command") depType0
    let (depVal1, depEnv1) = (cmd, over currentScope (+1) env')
    ((cmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ show (BaseType "%command") ++ ", got " ++ show depType1) $ depType1 == BaseType "%command"
    return ("{\n" ++ indent (intercalate "\n" declsS ++ "\n\n" ++ cmdS) ++ "\n}", BaseType "%command")

generateCodeASTCommand (ASTSeq cmds) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cmds, env)
    ((cmdsS, env'), depType0) <- runEval (evalFold True depVal0) depEnv0
    require ("Expected " ++ show (BaseType "%command") ++ "'s, got " ++ show depType0) $ all (==BaseType "%command") depType0
    let output = (intercalate "\n" cmdsS, env')
    assign volatileState $ snd output
    return (fst output, BaseType "%command")

generateCodeASTCommand (ASTPass) = do
    env <- use volatileState
    return ("", BaseType "%command")

generateCodeASTDecl (ASTDeclConst name val) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (val, env)
    ((valS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    (var, env') <- addVar name (True, True) (BaseType "int") env
    let output = ("int " ++ _cName var ++ " = " ++ valS ++ ";", env')
    assign volatileState $ snd output
    return (fst output, BaseType "%command")

generateCodeASTDecl (ASTDeclVar name Nothing) = do
    env <- use volatileState
    (var, env') <- addVar name (False, False) (BaseType "int") env
    let output = ("int " ++ _cName var ++ ";", env')
    assign volatileState $ snd output
    return (fst output, BaseType "%command")

generateCodeASTDecl (ASTDeclVar name (Just val)) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (val, env)
    ((valS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "int") ++ ", got " ++ show depType0) $ depType0 == BaseType "int"
    (var, env') <- addVar name (True, False) (BaseType "int") env
    let output = ("int " ++ _cName var ++ " = " ++ valS ++ ";", env')
    assign volatileState $ snd output
    return (fst output, BaseType "%command")

