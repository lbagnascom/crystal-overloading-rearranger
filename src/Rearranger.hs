module Rearranger where

import Data.Bifunctor (first)
import Data.List (permutations)
import Parser

isSlot :: Function -> Bool
isSlot f = funAnnotation f == Just "Slot"

slots :: Stmt -> [Function]
slots (ClassStmt c) = filter isSlot $ methods c
slots (FunctionStmt f) = if isSlot f then [f] else []
slots (UndiscoveredStmt _) = []

rearrangeSlots :: CrystalProgram -> [CrystalProgram]
rearrangeSlots cr = map (fst . replaceSlots cr) (permutations $ concatMap slots cr)

replaceSlots :: CrystalProgram -> [Function] -> (CrystalProgram, [Function])
replaceSlots [] fs = ([], fs)
replaceSlots cr [] = (cr, [])
replaceSlots (s : ss) (f : fs) = case s of
  ClassStmt sClass ->
    let (newMethods, fs1) = replaceFuns (methods sClass) (f : fs)
        newClassStmt = ClassStmt $ sClass {methods = newMethods}
        (ss2, fs2) = replaceSlots ss fs1
     in (newClassStmt : ss2, fs2)
  FunctionStmt f2 ->
    if isSlot f2
      then
        let (ss2, fs2) = replaceSlots ss fs
         in (FunctionStmt f : ss2, fs2)
      else
        let (ss2, fs2) = replaceSlots ss (f : fs)
         in (s : ss2, fs2)
  UndiscoveredStmt _ ->
    replaceSlots ss (f : fs)

replaceFuns :: [Function] -> [Function] -> ([Function], [Function])
replaceFuns [] fs = ([], fs)
replaceFuns fs [] = (fs, [])
replaceFuns (x : xs) (y : ys) =
  if isSlot x
    then first (y :) (replaceFuns xs ys)
    else first (x :) (replaceFuns xs (y : ys))
