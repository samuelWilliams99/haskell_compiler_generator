module SemanticsValidator where

import ParserRequirements
import Semantics
import Data.List
import Control.Lens
import Data.HashMap.Strict

validateSemantics :: SemanticsDef -> Result SemanticsDef
validateSemantics def = do
    newRules <- mapM (validateRuleTypes $ keys (_semanticsBaseTypes def)) (_semanticsRules def)

    return $ def{ _semanticsRules=newRules }

isVarType :: SemanticsDepOutputType -> Bool
isVarType (RawSemanticsDepType (SemanticsVarType _)) = True
isVarType _ = False

getVarStr :: SemanticsType -> String
getVarStr (SemanticsVarType s) = s
getVarStr (SemanticsStaticType s) = s
getVarStr (SemanticsStaticBaseType s) = s
getVarStr SemanticsCommandType = "%command"

validateBaseVars :: [String] -> SemanticsRuleDependency -> Result SemanticsRuleDependency
validateBaseVars baseTypes dep = let (RawSemanticsDepType v) = _semanticsDepOutputType dep
                                     name = getVarStr v
                                 in if elem name baseTypes then
                                     return $ dep { _semanticsDepOutputType=BuiltSemanticsDepTypeCompare v }
                                 else Error $ "Base type " ++ name ++ " is invalid"

validateTypeVars :: [String] -> [SemanticsRuleDependency] -> [SemanticsRuleDependency]
validateTypeVars seen (d@(SemanticsRuleDependency _ _ (RawSemanticsDepType (SemanticsVarType name)) _ _):ds) =
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
    let (varTypeDeps, baseTypeDeps) = partition (isVarType . _semanticsDepOutputType) deps

    newBaseTypeDeps <- mapM (validateBaseVars $ "%command":baseTypes) baseTypeDeps

    let newVarTypeDeps = validateTypeVars [] varTypeDeps

    let newDeps = newBaseTypeDeps ++ newVarTypeDeps

    mapM (\x -> if elem x baseTypes then return () else Error $ "Type " ++ x ++ " is not a base type" )
        $ rule ^. semanticsRuleTypeRestrictions . traverse . semanticsTypeResOptions

    return rule{ _semanticsRuleDeps=newDeps }
