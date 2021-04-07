module MtGeneratedSemantics (runSemantics) where

import MtGeneratedParser
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Lens
import ParserRequirements

import Data.HashMap.Strict hiding (map)
import Data.List
import Control.Lens
import MtStandardEnv
getVar :: String -> MTState -> Maybe MTVar
getVar name env = env ^? vars . at name . _Just . _head
getFunc :: String -> [String] -> MTState -> Maybe MTFunc
getFunc name args env = env ^. funcs . at (name, args)
standardState :: MTState
standardState = MTState empty 0 mtStandardEnv
makeDefined :: String -> MTState -> MTState
makeDefined name = vars . at name . _Just . _head . defined .~ True
addVar :: String -> Bool -> Bool -> MTState -> StateResult (String, MTState)
addVar name defined const env = do
    newName <- getNewName
    let newVar = MTVar name newName defined (env ^. currentScope) const
    return (newName, over vars (insertWith (++) name [newVar]) env)

class SemanticsEvaluable a where
    eval :: a -> StateResult (String, String)

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

evalFold :: (SemanticsEvaluable a) => Bool -> [a] -> StateResult ([String], [String])
evalFold _ [] = return ([], [])
evalFold forwardEnv (e:es) = do
    env <- gets _volatileState
    ((code, env'), t) <- runEval (eval e) env
    if forwardEnv then setVolatile env' else return ()
    (codes, ts) <- evalFold forwardEnv es
    return (code:codes, t:ts)

runEval :: StateResult (a, a) -> VolatileState -> StateResult ((a, VolatileState), a)
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

runSemantics :: (SemanticsEvaluable a) => a -> Result String
runSemantics input = do
    ((code, _), _) <- runStateT (eval input) defaultSemanticState
    let wrappedCode = "int main() {\n" ++ indent code ++ "\n    return 0;\n}"
    return $ (preCode) ++ "\n\n" ++ wrappedCode


data PersistentState = PersistentState{ _nameCounter :: Int }
type VolatileState = MTState
data SemanticsState = SemanticsState{ _persistentState :: PersistentState, _volatileState :: VolatileState }
type StateResult a = StateT SemanticsState Result a
defaultSemanticState = SemanticsState (PersistentState 0) $ standardState
makeLenses ''SemanticsState
makeLenses ''PersistentState

generateCodeASTExpr :: ASTExpr -> StateResult (String, String)

instance SemanticsEvaluable ASTExpr where
    eval = generateCodeASTExpr

generateCodeASTCommand :: ASTCommand -> StateResult (String, String)

instance SemanticsEvaluable ASTCommand where
    eval = generateCodeASTCommand

generateCodeASTDecl :: ASTDecl -> StateResult (String, String)

instance SemanticsEvaluable ASTDecl where
    eval = generateCodeASTDecl



isBaseType :: String -> Bool
isBaseType t = elem t ["int","boolean"]

generateCodeASTExpr (ASTExprBinOp op x1 x2) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x1, env)
    ((x1s, _), t1) <- runEval (eval depVal0) depEnv0
    let (depVal1, depEnv1) = (x2, env)
    ((x2s, _), t2) <- runEval (eval depVal1) depEnv1
    funcData <- forceMaybe ("Operator " ++ op ++ " does not exist for types " ++ t1 ++ " and " ++ t2) $ getFunc op [t1, t2] env
    let rType = retType funcData
    return ("(" ++ x1s ++ ") " ++ fName funcData ++ " (" ++ x2s ++ ")", rType)

generateCodeASTExpr (ASTExprInt x) = do
    env <- use volatileState
    return (show x, "int")

generateCodeASTExpr (ASTExprBool b) = do
    env <- use volatileState
    return (if b then "1" else "0", "boolean")

generateCodeASTExpr (ASTExprVar name) = do
    env <- use volatileState
    var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    require ("Variable " ++ name ++ " defined but not initialised") $ _defined var
    return (_cName var, "int")

generateCodeASTExpr (ASTExprUnOp "!" x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ "boolean" ++ ", got " ++ depType0) $ depType0 == "boolean"
    return ("!(" ++ xs ++ ")", "boolean")

generateCodeASTExpr (ASTExprUnOp "-" x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ "int" ++ ", got " ++ depType0) $ depType0 == "int"
    return ("-(" ++ xs ++ ")", "int")

generateCodeASTCommand (ASTAssign name x) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (x, env)
    ((xs, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ "int" ++ ", got " ++ depType0) $ depType0 == "int"
    var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    require "Cannot assign to a constant variable" $ not $ _const var
    let env' = if not $ _defined var then makeDefined name env else env
    let output = (_cName var ++ " = " ++ xs ++ ";", env')
    assign volatileState $ snd output
    return (fst output, "%command")

generateCodeASTCommand (ASTCall name args) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (args, env)
    ((argsS, _), types) <- runEval (evalFold False depVal0) depEnv0
    funcData <- forceMaybe ("Function " ++ name ++ " does not exist for args " ++ (intercalate ", " types)) $ getFunc name types env
    return (fName funcData ++ "(" ++ intercalate ", " argsS ++ ");", "%command")

generateCodeASTCommand (ASTIf cond tCmd fCmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cond, env)
    ((condS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ "boolean" ++ ", got " ++ depType0) $ depType0 == "boolean"
    let (depVal1, depEnv1) = (tCmd, env)
    ((tCmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ "%command" ++ ", got " ++ depType1) $ depType1 == "%command"
    let (depVal2, depEnv2) = (fCmd, env)
    ((fCmdS, _), depType2) <- runEval (eval depVal2) depEnv2
    require ("Expected " ++ "%command" ++ ", got " ++ depType2) $ depType2 == "%command"
    return ("if( " ++ condS ++ " ){\n" ++ indent tCmdS ++ "\n} else {\n" ++ indent fCmdS ++ "\n}", "%command")

generateCodeASTCommand (ASTWhile cond cmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cond, env)
    ((condS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ "boolean" ++ ", got " ++ depType0) $ depType0 == "boolean"
    let (depVal1, depEnv1) = (cmd, env)
    ((cmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ "%command" ++ ", got " ++ depType1) $ depType1 == "%command"
    return ("while( " ++ condS ++ " ){\n" ++ indent cmdS ++ "\n}", "%command")

generateCodeASTCommand (ASTLet decls cmd) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (decls, env)
    ((declsS, env'), depType0) <- runEval (evalFold True depVal0) depEnv0
    require ("Expected " ++ "%command" ++ "'s, got " ++ show depType0) $ all (=="%command") depType0
    let (depVal1, depEnv1) = (cmd, over currentScope (+1) env')
    ((cmdS, _), depType1) <- runEval (eval depVal1) depEnv1
    require ("Expected " ++ "%command" ++ ", got " ++ depType1) $ depType1 == "%command"
    return ("{\n" ++ indent (intercalate "\n" declsS ++ "\n\n" ++ cmdS) ++ "\n}", "%command")

generateCodeASTCommand (ASTSeq cmds) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (cmds, env)
    ((cmdsS, env'), depType0) <- runEval (evalFold True depVal0) depEnv0
    require ("Expected " ++ "%command" ++ "'s, got " ++ show depType0) $ all (=="%command") depType0
    let output = (intercalate "\n" cmdsS, env')
    assign volatileState $ snd output
    return (fst output, "%command")

generateCodeASTCommand (ASTPass) = do
    env <- use volatileState
    return ("", "%command")

generateCodeASTDecl (ASTDeclConst name val) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (val, env)
    ((valS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ "int" ++ ", got " ++ depType0) $ depType0 == "int"
    (cName, env') <- addVar name True True env
    let output = ("int " ++ cName ++ " = " ++ valS ++ ";", env')
    assign volatileState $ snd output
    return (fst output, "%command")

generateCodeASTDecl (ASTDeclVar name Nothing) = do
    env <- use volatileState
    (cName, env') <- addVar name False False env
    let output = ("int " ++ cName ++ ";", env')
    assign volatileState $ snd output
    return (fst output, "%command")

generateCodeASTDecl (ASTDeclVar name (Just val)) = do
    env <- use volatileState
    let (depVal0, depEnv0) = (val, env)
    ((valS, _), depType0) <- runEval (eval depVal0) depEnv0
    require ("Expected " ++ "int" ++ ", got " ++ depType0) $ depType0 == "int"
    (cName, env') <- addVar name True False env
    let output = ("int " ++ cName ++ " = " ++ valS ++ ";", env')
    assign volatileState $ snd output
    return (fst output, "%command")

