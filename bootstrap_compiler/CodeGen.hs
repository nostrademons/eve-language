module CodeGen(codegen) where
import Data.Maybe
import Data.Word
import List
import Monad
import LLVM.Core

import Expr
import Literal

externalizeStrings :: [FileLine] -> CodeGenModule [(String, Global (Array n Word8))]
externalizeStrings lines = liftM (zip strings) $ mapM createStringNul strings
  where
    strings = nub $ concatMap extractFromLine lines
    extractFromLine (FileLine (NakedExpr expr) _) = extractFromExpr expr
    -- TODO: extract from defs too
    extractFromLine _ = []
    extractFromExpr = extractFromExprVal . exprVal
    extractFromCond (pred, result) = extractFromExpr pred ++ extractFromExpr result
    extractFromExprVal (Literal (LitString s)) = [s]
    extractFromExprVal (Literal _) = []
    extractFromExprVal (TupleLiteral exprList) = concatMap extractFromExpr exprList
    extractFromExprVal (RecordLiteral pairList) = concatMap (extractFromExpr . snd) pairList
    extractFromExprVal (Variable _) = []
    extractFromExprVal (Cond alternatives) = concatMap (extractFromCond) alternatives
    extractFromExprVal (Funcall fn args) = extractFromExpr fn ++ concatMap extractFromExpr args
    extractFromExprVal (Lambda _ body) = extractFromExpr body

compileString :: [(String, Global (Array n Word8))] -> String
    -> CodeGenFunction r (Value (Ptr Word8))
compileString strings string = case lookup string strings of
  Nothing -> error $ string ++ " not properly externalized in " ++ show strings
  Just ptr -> getElementPtr0 ptr (0::Word32, ())

buildModule :: [FileLine] -> CodeGenModule ()
buildModule lines = do
  strings <- externalizeStrings lines
  -- TODO: Temporary, will only compile one program
  main <- newNamedFunction ExternalLinkage "main" :: TFunction (IO Word32)
  defineFunction main $ do
    msg <- compileString strings "Hello, World!" :: CodeGenFunction Word32 (Value (Ptr Word8))
    print <- externFunction "puts" :: CodeGenFunction Word32 (Function (Ptr Word8 -> IO Word32))
    call print msg
    ret (0::Word32)

codegen :: [FileLine] -> IO Module
codegen lines = do
  mod <- newModule
  defineModule mod $ buildModule lines
  return mod
