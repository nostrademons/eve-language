module CodeGen(codegen) where
import Control.Monad.State
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.ForeignPtr
import List
import Monad
import LLVM.Core
import qualified LLVM.FFI.Core as FFI

import Expr
import Literal
import SourcePos

data CompileState = CompileState {
    cs_module :: FFI.ModuleRef,
    cs_builder :: ForeignPtr FFI.Builder
}

type CompileM = StateT CompileState IO

runCompile :: CompileM a -> IO FFI.ModuleRef
runCompile action = do
  -- TODO: All the module setup, externs, etc.
  builderPtr <- FFI.createBuilder
  builder <- newForeignPtr FFI.ptrDisposeBuilder builderPtr
  modulePtr <- withCString "TODO_Extract_From_Module" FFI.moduleCreateWithName
  evalStateT action $ CompileState modulePtr builder
  return modulePtr
  
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

-- | Collects all "raw" expressions into a main() function that can be compiled
-- like any other.  Returns the original module contents with all of them
-- removed, and a new Definition for main() added.
buildMainFunction :: [FileLine] -> [FileLine]
buildMainFunction lines = if null exprs then notExprs 
    else buildMain (map extractExpr exprs) : notExprs
  where
    (exprs, notExprs) = partition isExpr lines
    isExpr (FileLine (NakedExpr expr) _) = True
    isExpr _ = False
    extractExpr (FileLine (NakedExpr expr) _) = expr
    firstPos = pos $ exprs !! 0
    buildMain exprs = FileLine (Definition $ buildMainDef exprs) firstPos
    -- TODO: This creates a def main(args as List<String>), but when we compile
    -- it, it needs to be lowered to the int main(int argc, char** argv) that
    -- the OS expects.  Should we merge this function directly with codegen,
    -- or depend upon the runtime to handle boxing?
    buildMainDef exprs = DefLine (Def "main" (ArgList [] Nothing) "" Nothing $
                                  map buildDefLine exprs) firstPos
    buildDefLine expr = DefLine (Statement expr) $ pos expr

buildModule :: [FileLine] -> CodeGenModule ()
buildModule lines = do
  strings <- externalizeStrings lines
  lines <- return $ buildMainFunction lines
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
