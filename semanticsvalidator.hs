{-|
Module      : SemanticsValidator
Description : Ensures the @SemanticsDef@ outputted by the parser is well-formed.
Copyright   : (c) Samuel Williams, 2021
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

This file checks that any direct references to base types are valid, and performs converts the semantics rule types to actions to be taken by the code generator.
-}
module SemanticsValidator (validateSemantics) where

import ParserRequirements
import Semantics
import Data.List
import Control.Lens
import Data.HashMap.Strict

-- | Entry point to the validator, handles all listed above.
validateSemantics :: SemanticsDef -> Result SemanticsDef
validateSemantics def = do
    newRules <- mapM (validateRuleTypes $ keys (_semanticsBaseTypes def)) (_semanticsRules def)

    return $ def{ _semanticsRules=newRules }

isVarType :: SemanticsDepOutputType -> Bool
isVarType (RawSemanticsDepType (SemanticsVarType _)) = True
isVarType _ = False

isBaseType :: SemanticsDepOutputType -> Bool
isBaseType (RawSemanticsDepType (SemanticsStaticBaseType _)) = True
isBaseType _ = False

getVarStr :: SemanticsType -> String
getVarStr (SemanticsStaticType s) = s
getVarStr (SemanticsStaticBaseType s) = s
getVarStr SemanticsCommandType = ""

validateBaseVars :: [String] -> SemanticsRuleDependency -> Result SemanticsRuleDependency
validateBaseVars baseTypes dep = let (RawSemanticsDepType v@(SemanticsStaticBaseType name)) = _semanticsDepOutputType dep
                                 in if elem name baseTypes then
                                     return $ dep { _semanticsDepOutputType=BuiltSemanticsDepTypeCompare v }
                                 else Error $ "Base type " ++ name ++ " is invalid"

validateTypeVars :: [String] -> [SemanticsRuleDependency] -> [SemanticsRuleDependency]
validateTypeVars seen (d@(SemanticsRuleDependency _ _ _ _ (RawSemanticsDepType (SemanticsVarType name)) _):ds) =
    (d { _semanticsDepOutputType=oType }):rest
  where
    hasSeen = elem name seen
    oType = if hasSeen then BuiltSemanticsDepTypeCompare $ SemanticsVarType name
            else BuiltSemanticsDepTypeAssign name
    rest = validateTypeVars (if hasSeen then seen else name:seen) ds
validateTypeVars _ [] = []

validateRuleTypes :: [String] -> SemanticsRule -> Result SemanticsRule
validateRuleTypes baseTypes rule = do
    let deps = _semanticsRuleDeps rule
    let (varTypeDeps, nonVarDeps) = partition (isVarType . _semanticsDepOutputType) deps

    let (baseTypeDeps, restDeps) = partition (isBaseType . _semanticsDepOutputType) nonVarDeps

    newBaseTypeDeps <- mapM (validateBaseVars baseTypes) baseTypeDeps

    let newVarTypeDeps = validateTypeVars [] varTypeDeps

    let newRestDeps = fmap (over semanticsDepOutputType (\(RawSemanticsDepType v) -> BuiltSemanticsDepTypeCompare v)) restDeps

    let newDeps = newBaseTypeDeps ++ newVarTypeDeps ++ newRestDeps

    mapM (\x -> if elem x baseTypes then return () else Error $ "Type " ++ x ++ " is not a base type" )
        $ rule ^. semanticsRuleTypeRestrictions . traverse . semanticsTypeResOptions

    return rule{ _semanticsRuleDeps=newDeps }
