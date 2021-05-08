{-|
Module      : Semantics
Description : Data structures for formalised semantics of the input programming language.
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

This module creates lenses for all constructors within the data types.
The structures within this file are outputted by the .smt parser, and directly used to generate the semantics checker.
-}
{-# LANGUAGE TemplateHaskell #-}
module Semantics where

import Control.Lens
import Data.HashMap.Strict

-- | The entry point data type for a full semantics definition
data SemanticsDef =
    SemanticsDef{ _semanticsBaseTypes :: HashMap String String -- ^ Map from base type to equivalent C type
                , _semanticsParamTypes :: HashMap String String -- ^ Map from parametrised type to haskell code for C type generation
                , _semanticsStateExtra :: String -- ^ Haskell code for extra data on the state
                , _semanticsVarExtra :: String -- ^ Haskell code for extra data on variables
                , _semanticsStandardEnv :: String -- ^ Haskell code for generating the standard environment
                , _semanticsHasIncludes :: Bool -- ^ Whether or not the semantics definition supports includes
                , _semanticsRules :: [SemanticsRule] -- ^ List of semantic reductions
                , _semanticsAstTypes :: [String] -- ^ List of the data types to be pattern matched by the reductions
                } deriving Show

-- | Types of types outputted by the parser
data SemanticsType = SemanticsCommandType -- ^ Non-type, for commands rather than expressions
                   | SemanticsStaticType String -- ^ The haskell code for a @VarType@, used within the generated semantics checker
                   | SemanticsStaticBaseType String -- ^ A base type, to be wrapped in a @VarType@ after use
                   | SemanticsVarType String -- ^ Variable type, the name of a variable in the generated semantics checker, which is expected to be a @VarType@
                   deriving Eq

instance Show SemanticsType where
    show SemanticsCommandType = "CommandType"
    show (SemanticsStaticBaseType s) = "BaseType " ++ show s
    show (SemanticsStaticType s) = s
    show (SemanticsVarType s) = s

-- | A semantic reduction, specifying the input pattern, output code and types, and dependencies
data SemanticsRule =
    SemanticsRule{ _semanticsRulePattern :: String -- ^ Haskell pattern to match, expected to be a constructor of the data type in '_semanticsRuleType'
                 , _semanticsRuleOutput :: String -- ^ Haskell String containing the equivalent C code for this reduction
                 , _semanticsRuleOutputEnv :: String -- ^ Haskell variable name for the output environment of this reduction
                 , _semanticsRuleType :: SemanticsType -- ^ The output type of this reduction
                 , _semanticsRuleAstType :: String -- ^ The AST data type that this rule pattern matches
                 , _semanticsRuleWhere :: Maybe String -- ^ Optional where clause to be copied into the generated function
                 , _semanticsRuleDeps :: [SemanticsRuleDependency] -- ^ List of dependencies of this reduction, to be reduced before this one can complete
                 , _semanticsRuleTypeRestrictions :: [SemanticsTypeRestriction] -- ^ Restrictions placed on variable semantics types outputted by reducing '_semanticsRuleDeps'
                 } deriving Show

-- | Restriction placed on a variable semantics type, enforcing presence of this type within a specified list.
data SemanticsTypeRestriction = SemanticsTypeRestriction{ _semanticsTypeResName :: String -- ^ Name of the variable semantics type
                                                        , _semanticsTypeResOptions :: [String] -- ^ List of base types this type must be in
                                                        } deriving Show

-- | A reduction dependency
data SemanticsRuleDependency = SemanticsRuleDependency{ _semanticsDepInput :: String -- ^ The input token to be reduced
                                                      , _semanticsDepInputEnv :: String -- ^ The environment to reduce the input token under
                                                      , _semanticsDepOutput :: String -- ^ A name to assign the code generated from the input token to
                                                      , _semanticsDepOutputEnv :: String -- ^ A name to assign the new environment to
                                                      , _semanticsDepOutputType :: SemanticsDepOutputType -- ^ How to handle the output type
                                                      , _semanticsDepIterType :: SemanticsRuleDependencyIterType -- ^ The iteration type of the dependency
                                                      } deriving Show

-- | Iteration type of a reduction dependency
data SemanticsRuleDependencyIterType = SemanticsDepSingle -- ^ The input token is already an evaluable, no fold needed
                                     | SemanticsDepFold Bool -- ^ Fold over a @Foldable@ structure, using the boolean flag to specify the environment is passed down
                                     deriving Show

-- | A type describing how to handle the output type of a dependency. The parser will output with this always being a @RawSemanticsDepType@,
-- then "SemanticsValidator" will convert these to the actions to take
data SemanticsDepOutputType = RawSemanticsDepType SemanticsType -- ^ The semantics type outputted directly from the parser
                            | BuiltSemanticsDepTypeAssign String -- ^ Assign the reductions output type to a variable under the given name
                            | BuiltSemanticsDepTypeCompare SemanticsType -- ^ Compare the reductions output type to the @SemanticsType@ given
                            deriving Show

makeLenses ''SemanticsDef
makeLenses ''SemanticsRule
makeLenses ''SemanticsTypeRestriction
makeLenses ''SemanticsRuleDependency
