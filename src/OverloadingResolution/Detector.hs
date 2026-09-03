{-# LANGUAGE NamedFieldPuns #-}

module OverloadingResolution.Detector where

import AstTypes
  ( Callsite (Callsite, callsiteArgs, callsiteFunName),
    Class (classSuper),
    Expr (ECall, ELiteral, ENew),
    Function (funArgs, funName),
    FunctionArg (FunctionArg, argType),
    Literal (LitBool, LitInt, LitString),
    Stmt (FunctionStmt),
    TypeRef (TypeRef, tRefType),
  )
import TypeResolution.Fix (Fix (Fix), fromFix)
import TypeResolution.Resolver (FixType, ResolvedAst, Type (TBool, TClass, TInt, TString))

matchingCallsite :: ResolvedAst -> Callsite FixType -> [Function FixType]
matchingCallsite ast (Callsite {callsiteFunName, callsiteArgs}) =
  let matchesCallsite f =
        funName f == callsiteFunName
          && argsMatch callsiteArgs (funArgs f)
   in foldr
        ( \stmt rec ->
            case stmt of
              (FunctionStmt f) -> if matchesCallsite f then f : rec else rec
              _ -> rec
        )
        []
        ast

argsMatch :: [Expr FixType] -> [FunctionArg FixType] -> Bool
argsMatch exprs fargs =
  length exprs == length fargs && and (zipWith exprMatchesArg exprs fargs)

exprMatchesArg :: Expr FixType -> FunctionArg FixType -> Bool
exprMatchesArg _ (FunctionArg {argType = Nothing}) = True
exprMatchesArg expr (FunctionArg {argType = Just tr}) =
  case (expr, fromFix $ tRefType tr) of
    (ELiteral (LitInt _), TInt) ->
      True
    (ELiteral (LitBool _), TBool) ->
      True
    (ELiteral (LitString _), TString) ->
      True
    (ENew (TypeRef {tRefType = Fix (TClass c1)}), TClass c2) ->
      c1 == c2 || c1 `isSubclassOf` c2
    (ECall _, _) ->
      error "Won't be using calls as expressions yet"
    _ ->
      False

destroyClass :: FixType -> Class FixType
destroyClass (Fix (TClass c)) = c
destroyClass _ = error "Expecting a TClass"

superclass :: Class FixType -> Class FixType
superclass c = case tRefType $ classSuper c of
  Fix (TClass sc) -> sc
  _ -> error "Superclass of every class should be a TClass"

isSubclassOf :: Class FixType -> Class FixType -> Bool
isSubclassOf c1 c2 =
  let sc = superclass c1
   in sc == c2 || sc `isSubclassOf` c2
