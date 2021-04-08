%extension mt
%precode {
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
    modEnv $ addStaticFunc "print" [BaseType "int"] (BaseType "%command") "printInt"
    modEnv $ addStaticFunc "print" [BaseType "boolean"] (BaseType "%command") "printInt"
    mapM (addOp "int" "int") intOps
    mapM (addOp "int" "boolean") intCompOps
    mapM (addOp "boolean" "boolean") boolCompOps
}
%outputprecode preCode
%basetype int "int"
%basetype boolean "int"
%varextra { (Bool, Bool) }
%standardenv makeStandardEnv

%asttype ASTExpr

case { ASTExprBinOp op x1 x2 } -> { "(" ++ x1s ++ ") " ++ _varCName var ++ " (" ++ x2s ++ ")" } @ rType
    evaluating
        x1 -> x1s @ t1
        x2 -> x2s @ t2
    where
        {
            var <- forceMaybe ("Operator " ++ op ++ " does not exist for types " ++ show t1 ++ " and " ++ show t2) $ getStaticFunc op [t1, t2] env
            let rType = var ^. varType
        }

case { ASTExprInt x } -> { show x } @ "int"

case { ASTExprBool b } -> { if b then "1" else "0" } @ "boolean"

case { ASTExprVar name } -> { _varCName var } @ "int"
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

case { ASTAssign name x } => { (_varCName var ++ " = " ++ xs ++ ";", env') }
    evaluating
        x -> xs @ "int"
    where
        {
            var <- forceMaybe ("No such variable " ++ name) $ getVar name env
            require "Cannot assign to a constant variable" $ not $ _const var
            let env' = if not $ _defined var then makeDefined var env else env
        }

case { ASTCall name args } -> { _varCName var ++ "(" ++ intercalate ", " argsS ++ ");" }
    evaluating
        args *-> argsS @ types
    where
        {
            var <- forceMaybe ("Function " ++ name ++ " does not exist for args " ++ (intercalate ", " $ fmap show types)) $ getStaticFunc name types env
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

case { ASTLet decls cmd } => { ("{\n" ++ indent (intercalate "\n" declsS ++ "\n\n" ++ cmdS) ++ "\n}", decreaseScope env'') }
    evaluating
        { (decls, env) } ^=> { (declsS, env') }
        { (cmd, increaseScope env') } => { (cmdS, env'') }

case { ASTSeq cmds } => { (intercalate "\n" cmdsS, env') }
    evaluating
        { (cmds, env) } ^=> { (cmdsS, env') }

case { ASTPass } -> { "" }

%asttype ASTDecl

case { ASTDeclConst name val } => { ("int " ++ _varCName var ++ " = " ++ valS ++ ";", env') }
    evaluating
        val -> valS @ "int"
    where
        {
            (var, env') <- addVar name (True, True) (BaseType "int") env
        }

case { ASTDeclVar name Nothing } => { ("int " ++ _varCName var ++ ";", env') }
    where
        {
            (var, env') <- addVar name (False, False) (BaseType "int") env
        }

case { ASTDeclVar name (Just val) } => { ("int " ++ _varCName var ++ " = " ++ valS ++ ";", env') }
    evaluating
        val -> valS @ "int"
    where
        {
            (var, env') <- addVar name (True, False) (BaseType "int") env
        }
