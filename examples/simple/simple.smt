%extension simple
%precode {
    preCode :: String
    preCode = "#include <stdio.h>"
}
%outputprecode preCode
%basetype integer "int"
%standardenv { return () }

%asttype SimpleCommands

case { SimpleCommands cmds ps } -> { cSeq cmdsS }
  evaluating
    cmds ^-> cmdsS

%asttype SimpleCommand

case { SimpleAssign name exp ps } => { (cCreateVar var (Just expS), env') }
  evaluating
    exp -> expS @ "integer"
  where
    {
        (var, env') <- addVar name () (BaseType "integer") env
    }

case { SimplePrint exp ps } -> { "printf(\"%d\\n\", " ++ expS ++ ");" }
  evaluating
    exp -> expS @ "integer"

%asttype SimpleExpression

case { SimpleInt v ps } -> { cInt v } @ "integer"

case { SimpleVar name ps } -> { cVar var } @ "integer"
  where
    {
        var <- forceMaybe ("No such variable " ++ name) $ getVar name env
    }