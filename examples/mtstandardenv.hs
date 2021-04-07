module MtStandardEnv where

import Data.HashMap.Strict hiding (map)
import Data.List hiding (union)
import Control.Lens

data MTVar = MTVar{ _name :: String
                  , _cName :: String
                  , _defined :: Bool
                  , _scopeLevel :: Int
                  , _const :: Bool
                  } deriving Show

instance Eq MTVar where
    a == b = (_cName a) == (_cName b)

data MTFunc = MTFunc{ fName :: String
                    , retType :: String
                    } deriving Show

data MTState = MTState{ _vars :: HashMap String [MTVar]
                      , _currentScope :: Int
                      , _funcs :: HashMap (String, [String]) MTFunc
                      }

preCode :: String
preCode = unlines [
    "#include <stdio.h>",
    "void printInt(int x) { printf(\"%d\\n\", x); }"
    ]

makeLenses ''MTVar
makeLenses ''MTState

mtStandardEnv :: HashMap (String, [String]) MTFunc
mtStandardEnv = union funcsEnv opsEnv

funcsEnv :: HashMap (String, [String]) MTFunc
funcsEnv = fromList [
    ( ("print", ["int"] ), MTFunc "printInt" "%command"),
    ( ("print", ["boolean"] ), MTFunc "printInt" "%command")
    ]

intOps :: [String]
intOps = ["^", "*", "/", "+", "-"]

intCompOps :: [String]
intCompOps = ["<", "<=", "==", "!=", ">=", ">"]

boolCompOps :: [String]
boolCompOps = ["==", "&&", "||"]

opsEnv :: HashMap (String, [String]) MTFunc
opsEnv = fromList $ (fmap (\op -> ((op, ["int", "int"] ), MTFunc op "int")) intOps) ++
                    (fmap (\op -> ((op, ["int", "int"] ), MTFunc op "boolean")) intCompOps) ++
                    (fmap (\op -> ((op, ["boolean", "boolean"] ), MTFunc op "boolean")) boolCompOps)