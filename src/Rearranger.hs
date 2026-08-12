module Rearranger where

import AstTypes
  ( AST,
    Class (classMethods),
    Function (funAnnotation),
    FunctionAnnotation (FunctionAnnotation),
    Module (moduleMethods),
    Stmt (ClassStmt, FunctionStmt, ModuleStmt),
  )
import Data.Bifunctor (first)
import Data.List (permutations)

isSlot :: Function t -> Bool
isSlot f = funAnnotation f == Just (FunctionAnnotation "Slot")

slots :: Stmt t -> [Function t]
slots (ClassStmt c) = filter isSlot $ classMethods c
slots (ModuleStmt c) = filter isSlot $ moduleMethods c
slots (FunctionStmt f) = if isSlot f then [f] else []

rearrangeSlots :: AST t -> [AST t]
rearrangeSlots cr = map (fst . replaceSlots cr) (permutations $ concatMap slots cr)

replaceSlots :: AST t -> [Function t] -> (AST t, [Function t])
replaceSlots [] fs = ([], fs)
replaceSlots cr [] = (cr, [])
replaceSlots (s : ss) (f : fs) = case s of
  ClassStmt sClass ->
    let (newMethods, fs1) = replaceFuns (classMethods sClass) (f : fs)
        newClassStmt = ClassStmt $ sClass {classMethods = newMethods}
        (ss2, fs2) = replaceSlots ss fs1
     in (newClassStmt : ss2, fs2)
  ModuleStmt sModule ->
    let (newMethods, fs1) = replaceFuns (moduleMethods sModule) (f : fs)
        newModuleStmt = ModuleStmt $ sModule {moduleMethods = newMethods}
        (ss2, fs2) = replaceSlots ss fs1
     in (newModuleStmt : ss2, fs2)
  FunctionStmt f2 ->
    if isSlot f2
      then
        let (ss2, fs2) = replaceSlots ss fs
         in (FunctionStmt f : ss2, fs2)
      else
        let (ss2, fs2) = replaceSlots ss (f : fs)
         in (s : ss2, fs2)

replaceFuns :: [Function t] -> [Function t] -> ([Function t], [Function t])
replaceFuns [] fs = ([], fs)
replaceFuns fs [] = (fs, [])
replaceFuns (x : xs) (y : ys) =
  if isSlot x
    then first (y :) (replaceFuns xs ys)
    else first (x :) (replaceFuns xs (y : ys))
