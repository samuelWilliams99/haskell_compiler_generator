module SimpleSemantics (runSemantics, rDefaultState, _outPreCode, indent, VolatileState (..), PersistentState (..), SemanticsState (..), Var (..)) where

import SimpleParser
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Lens
import ParserRequirements
import Data.HashMap.Strict
import Data.List hiding (union)
import Data.Hashable
import Data.Maybe
import Data.Monoid


isBaseType :: String -> Bool
isBaseType t = elem t ["integer"]
baseTypesC :: HashMap String String
baseTypesC = fromList [("integer","int")]
paramTypesC :: HashMap String ([String] -> String)
paramTypesC = fromList []

type StateExtra = ()
type VarExtra = ()
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
instance Semigroup VolatileState where
    (VolatileState vs1 _ fs1 e1) <> (VolatileState vs2 _ fs2 e2) = VolatileState (union vs2 vs1) 0 (union fs2 fs1) (e1 <> e2)
instance Monoid VolatileState where
    mempty = VolatileState empty (-1) empty mempty
data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState, _psState :: ParseState }
type StateResult a = StateT SemanticsState Result a
defaultPersistentState = PersistentState 0
makeLenses ''SemanticsState
makeLenses ''PersistentState
makeLenses ''VolatileState
makeLenses ''Var
makeLenses ''VarType
rDefaultState :: Result SemanticsState
rDefaultState = fmap snd $ runStateT (return () >> modEnv increaseScope) (SemanticsState defaultPersistentState mempty $ parseState "")




_outPreCode :: String
_outPreCode =  "#include <stdio.h>"  ++ "\n\n"


class SemanticsEvaluable a where
    eval :: a -> StateResult (String, VarType)

incrementCounter :: StateResult Int
incrementCounter = do
   n <- use $ persistentState . nameCounter
   modifying (persistentState . nameCounter) (+1)
   return n
err :: String -> StateResult a
err errMsg = use psState >>= \ps -> lift $ Error $ errMsg ++ " at " ++ showPos ps

require :: String -> Bool -> StateResult ()
require _ True = return ()
require errMsg False = err errMsg

forceMaybe :: String -> Maybe a -> StateResult a
forceMaybe errMsg Nothing = err errMsg
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
getVarFunc :: String -> VolatileState -> Maybe (Var VarExtra, [VarType], VarType)
getVarFunc name env = do
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

runSemantics :: (SemanticsEvaluable a) => SemanticsState -> a -> Result (String, SemanticsState)
runSemantics inpState input = do
    ((code, _), s) <- runStateT (eval input) inpState
    return (code, s)


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
cIf :: [(String, String)] -> Maybe String -> String
cIf cs mElse = (concat $ fmap (\(cond, cmd) -> "if(" ++ cond ++ ")" ++ cBlock cmd ++ " else ") cs) ++ cBlock (fromMaybe "" mElse)
cSimpleIf :: String -> String -> String -> String
cSimpleIf cond t f = cIf [(cond, t)] (Just f)
cSeq :: [String] -> String
cSeq = intercalate "\n"
cPass :: String
cPass = ""
cCreateVar :: Var a -> Maybe String -> String
cCreateVar v Nothing = toCType (_varType v) ++ " " ++ cVar v ++ ";"
cCreateVar v (Just s) = toCType (_varType v) ++ " " ++ cVar v ++ " = " ++ s ++ ";"
cWhile :: String -> String -> String
cWhile cond cmd = "while(" ++ cond ++ ")" ++ cBlock cmd
cFor :: Var a -> String -> String -> String -> String -> String
cFor v init cond step cmd = "for(" ++ cCreateVar v (Just init) ++ "; " ++ cond ++ "; " ++ step ++ ")" ++ cBlock cmd
cSimpleFor :: Var a -> String -> String -> String -> String -> String
cSimpleFor v init limit step cmd = cBlock $ "int limit = " ++ limit ++ ";\n" ++
                                            cFor v init "limit" (cVar v ++ " += " ++ step) cmd


generateCodeSimpleCommands :: SimpleCommands -> StateResult (String, VarType)

instance SemanticsEvaluable SimpleCommands where
    eval = generateCodeSimpleCommands

generateCodeSimpleCommand :: SimpleCommand -> StateResult (String, VarType)

instance SemanticsEvaluable SimpleCommand where
    eval = generateCodeSimpleCommand

generateCodeSimpleExpression :: SimpleExpression -> StateResult (String, VarType)

instance SemanticsEvaluable SimpleExpression where
    eval = generateCodeSimpleExpression



generateCodeSimpleCommands (SimpleCommands cmds ps) = do
    env <- use volatileState
    assign psState ps
    let (depVal0, depEnv0) = (cmds, env)
    ((cmdsS, _), depType0) <- runEval (evalFold True depVal0) depEnv0
    require ("Expected " ++ show (CommandType) ++ "'s, got " ++ show depType0) $ all (==CommandType) depType0
    assign psState ps
    return (cSeq cmdsS, CommandType)

generateCodeSimpleCommand (SimpleAssign name exp ps) = do
    env <- use volatileState
    assign psState ps
    let (depVal0, depEnv0) = (exp, env)
    ((expS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "integer") ++ ", got " ++ show depType0) $ depType0 == BaseType "integer"
    assign psState ps
    (var, env') <- addVar name () (BaseType "integer") env
    assign volatileState env'
    return (cCreateVar var (Just expS), CommandType)

generateCodeSimpleCommand (SimplePrint exp ps) = do
    env <- use volatileState
    assign psState ps
    let (depVal0, depEnv0) = (exp, env)
    ((expS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ show (BaseType "integer") ++ ", got " ++ show depType0) $ depType0 == BaseType "integer"
    assign psState ps
    return ("printf(\"%d\\n\", " ++ expS ++ ");", CommandType)

generateCodeSimpleExpression (SimpleInt v ps) = do
    env <- use volatileState
    assign psState ps
    return (cInt v, BaseType "integer")

generateCodeSimpleExpression (SimpleVar name ps) = do
    env <- use volatileState
    assign psState ps
    var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    return (cVar var, BaseType "integer")

