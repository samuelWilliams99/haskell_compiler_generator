%extension mt
%precode {
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
}
%outputprecode preCode
%basetypes int boolean
%state MTState standardState

%asttype ASTExpr

case { ASTExprBinOp op x1 x2 } -> { "(" ++ x1s ++ ") " ++ fName funcData ++ " (" ++ x2s ++ ")" } @ rType
    evaluating
        x1 -> x1s @ t1
        x2 -> x2s @ t2
    where
        {
            funcData <- forceMaybe ("Operator " ++ op ++ " does not exist for types " ++ t1 ++ " and " ++ t2) $ getFunc op [t1, t2] env
            let rType = retType funcData
        }

case { ASTExprInt x } -> { show x } @ "int"

case { ASTExprBool b } -> { if b then "1" else "0" } @ "boolean"

case { ASTExprVar name } -> { _cName var } @ "int"
    where
        {
            var <- forceMaybe ("No such variable " ++ name) $ getVar name env
            require ("Variable " ++ name ++ " defined but not initialised") $ _defined var
        }

case { ASTExprUnOp "!" x } -> { "!(" ++ xs ++ ")" } @ "boolean"
    evaluating
        x -> xs @ "boolean"

case { ASTExprUnOp "-" x } -> { "-(" ++ xs ++ ")" } @ "int"
    evaluating
        x -> xs @ "int"

%asttype ASTCommand

case { ASTAssign name x } => { (_cName var ++ " = " ++ xs ++ ";", env') }
    evaluating
        x -> xs @ "int"
    where
        {
            var <- forceMaybe ("No such variable " ++ name) $ getVar name env
            require "Cannot assign to a constant variable" $ not $ _const var
            let env' = if not $ _defined var then makeDefined name env else env
        }

case { ASTCall name args } -> { fName funcData ++ "(" ++ intercalate ", " argsS ++ ");" }
    evaluating
        args *-> argsS @ types
    where
        {
            funcData <- forceMaybe ("Function " ++ name ++ " does not exist for args " ++ (intercalate ", " types)) $ getFunc name types env
        }

case { ASTIf cond tCmd fCmd } -> { "if( " ++ condS ++ " ){\n" ++ indent tCmdS ++ "\n} else {\n" ++ indent fCmdS ++ "\n}" }
    evaluating
        cond -> condS @ "boolean"
        tCmd -> tCmdS
        fCmd -> fCmdS

case { ASTWhile cond cmd } -> { "while( " ++ condS ++ " ){\n" ++ indent cmdS ++ "\n}" }
    evaluating
        cond -> condS @ "boolean"
        cmd -> cmdS

case { ASTLet decls cmd } -> { "{\n" ++ indent (intercalate "\n" declsS ++ "\n\n" ++ cmdS) ++ "\n}" }
    evaluating
        { (decls, env) } ^=> { (declsS, env') }
        { (cmd, over currentScope (+1) env') } => { (cmdS, _) }

case { ASTSeq cmds } => { (intercalate "\n" cmdsS, env') }
    evaluating
        { (cmds, env) } ^=> { (cmdsS, env') }

case { ASTPass } -> { "" }

%asttype ASTDecl

case { ASTDeclConst name val } => { ("int " ++ cName ++ " = " ++ valS ++ ";", env') }
    evaluating
        val -> valS @ "int"
    where
        {
            (cName, env') <- addVar name True True env
        }

case { ASTDeclVar name Nothing } => { ("int " ++ cName ++ ";", env') }
    where
        {
            (cName, env') <- addVar name False False env
        }

case { ASTDeclVar name (Just val) } => { ("int " ++ cName ++ " = " ++ valS ++ ";", env') }
    evaluating
        val -> valS @ "int"
    where
        {
            (cName, env') <- addVar name True False env
        }
