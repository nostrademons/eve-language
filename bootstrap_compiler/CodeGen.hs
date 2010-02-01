{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
  UndecidableInstances #-}
module CodeGen(codegen, writeBitcode) where
import Control.Monad.State
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import List
import Monad
import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.BitWriter as FFI
import System.IO.Unsafe

import Expr
import Literal
import SourcePos

data CompileState = CompileState {
  cs_gensym :: Integer,
  cs_module :: FFI.ModuleRef,
  cs_builder :: ForeignPtr FFI.Builder,
  cs_externs :: [(String, FFI.ValueRef)],
  cs_strings :: [(String, FFI.ValueRef)]
}

type CompileM = StateT CompileState IO

class BuilderArg a
instance BuilderArg FFI.ValueRef
instance BuilderArg FFI.TypeRef
instance BuilderArg CInt

class BuilderType b f where
  withBuilder :: (FFI.BuilderRef -> f) -> b

-- Base case: apply the builder and name and lift into compileM.
instance BuilderType (CompileM FFI.ValueRef) (CString -> IO FFI.ValueRef) where
  withBuilder f = do
    builder <- liftM cs_builder get
    liftIO $ withForeignPtr builder $ \b -> withCString "" $ \s -> f b s

-- Special base case: no name parameter.
instance BuilderType (CompileM FFI.ValueRef) (IO FFI.ValueRef) where
  withBuilder f = do
    builder <- liftM cs_builder get
    liftIO $ withForeignPtr builder $ \b -> f b

-- Recursive case: propagate the builder, apply the function to the arg.
instance (BuilderArg a, BuilderType b f) => BuilderType (a -> b) (a -> f) where
  withBuilder f arg = withBuilder (flip f $ arg)

-- Special case: a function with a Ptr and a CUInt takes an array, so we need
-- to convert a list into it with withArrayLen.
instance (Storable a, BuilderArg a, BuilderType b f) => 
    BuilderType ([a] -> b) (Ptr a -> CUInt -> f) where
  withBuilder f argList = withBuilder applyWithArrayLen
    where
      applyWithArrayLen builder = unsafePerformIO $ withArrayLen argList $ 
          \len ptr -> return $ f builder ptr (fromIntegral len)

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
    return $ FFI.structType ptr (fromIntegral len) 0

-- unsafePerformIO is safe because only effect is memory allocation.
functionType :: Bool -> [FFI.TypeRef] -> FFI.TypeRef -> FFI.TypeRef
functionType varargs args ret = unsafePerformIO $
  withArrayLen args $ \len ptr ->
    return $ FFI.functionType ret ptr (fromIntegral len) (fromBool varargs)

constInt :: Int -> FFI.ValueRef
constInt n = FFI.constInt FFI.int32Type (fromIntegral n) 1

externData = [
    ("print", ("puts", functionType False [cStringType] FFI.int32Type))
    ]

externalizeFunctions :: CompileM ()
externalizeFunctions = mapM createFunction cSignatures >>= setExterns
  where
    (names, cSignatures) = unzip externData
    createFunction (name, typ) = do
      modul <- getModule
      liftIO $ withCString name $ \cName -> do
        func <- FFI.addFunction modul cName typ
        FFI.setLinkage func 0
        return func
    setExterns functions = modify (\s -> s { cs_externs = zip names functions })
  
externalizeStrings :: [FileLine] -> CompileM ()
externalizeStrings lines = mapM createStringConstant strings >>= setStrings
  where
    createStringConstant str = do
      modul <- getModule
      withGensym $ \name -> do
        global <- FFI.addGlobal modul stringType name
        FFI.setLinkage global 0
        FFI.setGlobalConstant global 1
        withCStringLen str $ \(sPtr, sLen) -> do
          let llvmLen = constInt sLen
          let llvmStr = FFI.constString sPtr (fromIntegral sLen) 0
          withArrayLen [llvmLen, llvmStr] $ \len ptr ->
            FFI.setInitializer global $ FFI.constStruct ptr (fromIntegral len) 0
        return global
    strings = nub $ concatMap extractFromLine lines
    setStrings values = modify $ \s -> s { cs_strings = zip strings values }
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

compileFileLine :: FileLine -> CompileM FFI.ValueRef
compileFileLine line = compile $ fileLineVal line
  where
    compile (NakedExpr expr) = compileExpr expr
    compile _ = error "Definitions not implemented."

compileExpr :: Expr -> CompileM FFI.ValueRef
compileExpr expr = compile $ exprVal expr
  where
    compile (Literal (LitString str)) = compileString str
    compile (Literal _) = error "Non-string literals not implemented."
    compile (Funcall (Expr (Variable var) _ _) args) = do
      externs <- liftM cs_externs get
      argVals <- mapM compileExpr args
      case lookup var externs of
        Just func -> withBuilder FFI.buildCall func argVals
        Nothing -> error "User-defined functions not implemented."

compileString :: String -> CompileM FFI.ValueRef
compileString str = do
  strings <- liftM cs_strings get
  case lookup str strings of
    Nothing -> error $ str ++ " not properly externalized in " ++ show strings
    Just ptr -> withBuilder FFI.buildGEP ptr [constInt 0, constInt 1, constInt 0]

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

createFunction :: CompileM ()
createFunction = do
  modul <- getModule
  builder <- liftM cs_builder get
  liftIO $ withCString "eve_main" $ \name -> do
    let mainType = functionType False [] FFI.int32Type
    func <- FFI.addFunction modul name mainType
    FFI.setLinkage func 0
    entry <- withCString "entry" $ \name -> FFI.appendBasicBlock func name
    withForeignPtr builder $ \b -> FFI.positionAtEnd b entry

codegen :: [FileLine] -> IO FFI.ModuleRef
codegen lines = do
  builderPtr <- FFI.createBuilder
  builder <- newForeignPtr FFI.ptrDisposeBuilder builderPtr
  modulePtr <- withCString "TODO_Extract_From_Module" FFI.moduleCreateWithName
  let action = do
        externalizeFunctions
        externalizeStrings lines
        createFunction
        mapM compileFileLine lines
        withBuilder FFI.buildRet (constInt 0) :: (CompileM FFI.ValueRef)
  evalStateT action $ CompileState 0 modulePtr builder [] []
  return modulePtr

writeBitcode :: String -> FFI.ModuleRef -> IO ()
writeBitcode name modul = withCString name $ \cName -> do
  ret <- FFI.writeBitcodeToFile modul cName
  when (ret /= 0) $ ioError $ userError $
    "writeBitcodeToFile returned " ++ show ret
  return ()
