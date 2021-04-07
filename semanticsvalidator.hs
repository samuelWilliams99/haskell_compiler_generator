module SemanticsValidator where

import ParserRequirements
import Semantics
import Data.List
import Control.Lens

validateSemantics :: SemanticsDef -> Result SemanticsDef
validateSemantics def = do
    newRules <- mapM (validateRuleTypes $ _semanticsBaseTypes def) (_semanticsRules def)

    return $ def{ _semanticsRules=newRules }

isVarType :: SemanticsDepOutputType -> Bool
isVarType (RawSemanticsDepType (SemanticsVarType _)) = True
isVarType _ = False

getVarStr :: SemanticsDepOutputType -> String
getVarStr (RawSemanticsDepType (SemanticsVarType s)) = s
getVarStr (RawSemanticsDepType (SemanticsBaseType s)) = s
getVarStr (RawSemanticsDepType SemanticsCommandType) = "%command"

validateBasevars :: [String] -> SemanticsRuleDependency -> Result SemanticsRuleDependency
validateBasevars baseTypes dep = let name = getVarStr $ _semanticsDepOutputType dep in
                                 if elem name baseTypes then
                                     return $ dep { _semanticsDepOutputType=BuiltSemanticsDepTypeCompare $ SemanticsBaseType name }
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

    newBaseTypeDeps <- mapM (validateBasevars $ "%command":baseTypes) baseTypeDeps

    let newVarTypeDeps = validateTypeVars [] varTypeDeps

    let newDeps = newBaseTypeDeps ++ newVarTypeDeps

    mapM (\x -> if elem x baseTypes then return () else Error $ "Type " ++ x ++ " is not a base type" )
        $ rule ^. semanticsRuleTypeRestrictions . traverse . semanticsTypeResOptions

    return rule{ _semanticsRuleDeps=newDeps }
