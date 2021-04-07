module Semantics where

import Control.Lens

data SemanticsDef =
    SemanticsDef{ _semanticsPreCode :: String
                , _semanticsBaseTypes :: [String]
                , _semanticsState :: (String, String)
                , _semanticsRules :: [SemanticsRule]
                , _semanticsAstTypes :: [String]
                } deriving Show

data SemanticsType = SemanticsCommandType | SemanticsBaseType String | SemanticsVarType String

instance Show SemanticsType where
    show SemanticsCommandType = show "%command"
    show (SemanticsBaseType s) = show s
    show (SemanticsVarType s) = s

data SemanticsRule =
    SemanticsRule{ _semanticsRulePattern :: String
                 , _semanticsRuleOutput :: String
                 , _semanticsRuleChangesEnv :: Bool
                 , _semanticsRuleType :: SemanticsType
                 , _semanticsRuleAstType :: String
                 , _semanticsRuleWhere :: Maybe String
                 , _semanticsRuleDeps :: [SemanticsRuleDependency]
                 , _semanticsRuleTypeRestrictions :: [SemanticsTypeRestriction]
                 } deriving Show

data SemanticsTypeRestriction = SemanticsTypeRestriction{ _semanticsTypeResName :: String
                                                        , _semanticsTypeResOptions :: [String]
                                                        } deriving Show

data SemanticsRuleDependencyIterType = SemanticsDepSingle | SemanticsDepFold Bool deriving Show

data SemanticsRuleDependency = SemanticsRuleDependency{ _semanticsDepInput :: String
                                                      , _semanticsDepOutput :: String
                                                      , _semanticsDepOutputType :: SemanticsDepOutputType
                                                      , _semanticsDepUsesEnv :: Bool
                                                      , _semanticsDepIterType :: SemanticsRuleDependencyIterType
                                                      } deriving Show

data SemanticsDepOutputType =
    RawSemanticsDepType SemanticsType |
    BuiltSemanticsDepTypeAssign String |
    BuiltSemanticsDepTypeCompare SemanticsType
    deriving Show

makeLenses ''SemanticsDef
makeLenses ''SemanticsRule
makeLenses ''SemanticsTypeRestriction
makeLenses ''SemanticsRuleDependency
