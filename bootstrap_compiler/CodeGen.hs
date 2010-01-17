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
  cs_gensym :: Integer,
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
  evalStateT action $ CompileState 0 modulePtr builder
  return modulePtr

withGensym :: (CString -> IO a) -> CompileM a
withGensym f = do
  state <- get
  name <- return $ "__" ++ show (cs_gensym state)
  put (state { cs_gensym = cs_gensym state + 1 })
  liftIO $ withCString name f

getModule :: CompileM FFI.ModuleRef
getModule = liftM cs_module get

cStringType :: FFI.TypeRef
cStringType = FFI.pointerType FFI.int8Type 0

-- unsafePerformIO is safe because only effect is memory allocation.
stringType :: FFI.TypeRef
stringType = unsafePerformIO $ 
  withArrayLen [FFI.int32Type, FFI.arrayType FFI.int8Type 0] $ \len ptr ->
    return $ FFI.structType ptr len 0

-- unsafePerformIO is safe because only effect is memory allocation.
functionType :: Bool -> [FFI.TypeRef] -> FFI.TypeRef -> FFI.TypeRef
functionType varargs args ret = unsafePerformIO $
  withArrayLen args $ \len ptr ->
    return $ FFI.functionType ret ptr len varargs

externs = [("puts", functionType False [cStringType] FFI.int32Type)]

externalizeFunctions :: CompileM [(String, FFI.ValueRef)]
externalizeFunctions = liftM (zip names) $ mapM createFunction externs
  where
    (names, types) = unzip externs
    createFunction (name, typ) = do
      modul <- getModule
      liftIO $ withCString name $ \cName -> do
        func <- FFI.addFunction modul cName typ
        FFI.setLinkage func ExternalLinkage
        return func
  
externalizeStrings :: [FileLine] -> CompileM [(String, FFI.ValueRef)]
externalizeStrings lines = liftM (zip strings) $ mapM createStringConstant strings
  where
    createStringConstant str = do
      modul <- getModule
      withGensym $ \name ->
        global <- FFI.addGlobal modul name stringType
        FFI.setLinkage global (fromIntegral $ fromEnum PrivateLinkage)
        FFI.setGlobalConstant global 1
        withCStringLen str $ \(sPtr, sLen) -> do
          let cLen = fromIntegral sLen
          let llvmLen = FFI.constInt FFI.int32Type cLen 0
          let llvmStr = FFI.constString cLen 0
          withArrayLen [llvmLen, llvmStr] $ \len ptr ->
            FFI.setInitializer global $ FFI.constStruct  ptr (fromIntegral len) 0
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
