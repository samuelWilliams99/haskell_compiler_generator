%extension mt
%precode {
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
}
%outputprecode preCode
%basetype int "int"
%basetype boolean "int"
%varextra { (Bool, Bool) } { (True, False) }

%asttype ASTExpr

case { ASTExprBinOp op x1 x2 } -> { "(" ++ x1s ++ ") " ++ fName ++ " (" ++ x2s ++ ")" } @ rType
    evaluating
        x1 -> x1s @ t1
        x2 -> x2s @ t2
    where
        {
            (rType, fName) <- forceMaybe ("Operator " ++ op ++ " does not exist for types " ++ show t1 ++ " and " ++ show t2) $ getOp op [t1, t2] env
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
            let env' = if not $ _defined var then makeDefined var env else env
        }

case { ASTCall name args } -> { fName ++ "(" ++ intercalate ", " argsS ++ ");" }
    evaluating
        args *-> argsS @ types
    where
        {
            (_, fName) <- forceMaybe ("Function " ++ name ++ " does not exist for args " ++ (intercalate ", " $ fmap show types)) $ getFunc name types env
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

case { ASTDeclConst name val } => { ("int " ++ _cName var ++ " = " ++ valS ++ ";", env') }
    evaluating
        val -> valS @ "int"
    where
        {
            (var, env') <- addVar name (True, True) (BaseType "int") env
        }

case { ASTDeclVar name Nothing } => { ("int " ++ _cName var ++ ";", env') }
    where
        {
            (var, env') <- addVar name (False, False) (BaseType "int") env
        }

case { ASTDeclVar name (Just val) } => { ("int " ++ _cName var ++ " = " ++ valS ++ ";", env') }
    evaluating
        val -> valS @ "int"
    where
        {
            (var, env') <- addVar name (True, False) (BaseType "int") env
        }
