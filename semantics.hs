{-|
Module      : Semantics
Description : 
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release


-}
{-# LANGUAGE TemplateHaskell #-}
module Semantics where

import Control.Lens
import Data.HashMap.Strict

-- | 
data SemanticsDef =
    SemanticsDef{ _semanticsBaseTypes :: HashMap String String
                , _semanticsParamTypes :: HashMap String String
                , _semanticsStateExtra :: String
                , _semanticsVarExtra :: String
                , _semanticsStandardEnv :: String
                , _semanticsHasIncludes :: Bool
                , _semanticsRules :: [SemanticsRule]
                , _semanticsAstTypes :: [String]
                } deriving Show

-- | 
data SemanticsType = SemanticsCommandType | SemanticsStaticType String | SemanticsStaticBaseType String | SemanticsVarType String deriving Eq

instance Show SemanticsType where
    show SemanticsCommandType = "CommandType"
    show (SemanticsStaticBaseType s) = "BaseType " ++ show s
    show (SemanticsStaticType s) = s
    show (SemanticsVarType s) = s

-- |
data SemanticsRule =
    SemanticsRule{ _semanticsRulePattern :: String
                 , _semanticsRuleOutput :: String
                 , _semanticsRuleOutputEnv :: String
                 , _semanticsRuleType :: SemanticsType
                 , _semanticsRuleAstType :: String
                 , _semanticsRuleWhere :: Maybe String
                 , _semanticsRuleDeps :: [SemanticsRuleDependency]
                 , _semanticsRuleTypeRestrictions :: [SemanticsTypeRestriction]
                 } deriving Show

-- |
data SemanticsTypeRestriction = SemanticsTypeRestriction{ _semanticsTypeResName :: String
                                                        , _semanticsTypeResOptions :: [String]
                                                        } deriving Show

-- |
data SemanticsRuleDependencyIterType = SemanticsDepSingle | SemanticsDepFold Bool deriving Show

-- |
data SemanticsRuleDependency = SemanticsRuleDependency{ _semanticsDepInput :: String
                                                      , _semanticsDepInputEnv :: String
                                                      , _semanticsDepOutput :: String
                                                      , _semanticsDepOutputEnv :: String
                                                      , _semanticsDepOutputType :: SemanticsDepOutputType
                                                      , _semanticsDepIterType :: SemanticsRuleDependencyIterType
                                                      } deriving Show

-- |
data SemanticsDepOutputType =
    RawSemanticsDepType SemanticsType |
    BuiltSemanticsDepTypeAssign String |
    BuiltSemanticsDepTypeCompare SemanticsType
    deriving Show

makeLenses ''SemanticsDef
makeLenses ''SemanticsRule
makeLenses ''SemanticsTypeRestriction
makeLenses ''SemanticsRuleDependency
