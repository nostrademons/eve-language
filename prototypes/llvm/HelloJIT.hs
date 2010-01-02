module Main (main) where

import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

data HelloMod = HelloMod {
  mGreet :: Function (Word32 -> IO Word32),
  mFib :: Function (Word32 -> IO Word32),
  mMain :: Function (Word32 -> Ptr (Ptr Word8) -> IO Word32)
}

bldGreet :: CodeGenModule HelloMod
bldGreet = do
    printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> VarArgs Word32)
    atoi <- newNamedFunction ExternalLinkage "atoi" :: TFunction (Ptr Word8 -> IO Word32)
    fib <- newNamedFunction InternalLinkage "fib"
    greetz <- createStringNul "Hello, JIT #%d!\n"
    usage <- createStringNul "Usage: lli HelloJit.bc <number>.\n"
    func <- createFunction ExternalLinkage $ \x -> do
      fibResult <- call fib x

      tmp <- getElementPtr0 greetz (0::Word32, ())
      let p2 = castVarArgs printf :: Function (Ptr Word8 -> Word32 -> IO Word32)
      _ <- call p2 tmp fibResult -- Throw away return value.
      ret (1::Word32)

    defineFunction fib $ \x -> do
      recurse <- newBasicBlock
      exit <- newBasicBlock

      test <- icmp IntUGT x (2::Word32)
      condBr test recurse exit

      defineBasicBlock recurse
      x1 <- sub x (1::Word32)
      fibx1 <- call fib x1
      x2 <- sub x (2::Word32)
      fibx2 <- call fib x2
      r <- add fibx1 fibx2
      ret r

      defineBasicBlock exit
      ret (1::Word32)

    main <- newNamedFunction ExternalLinkage "main2"
    defineFunction main $ \argc argv -> do
      errBlock <- newBasicBlock
      run <- newBasicBlock

      test <- icmp IntNE argc (2::Word32)
      condBr test errBlock run

      defineBasicBlock errBlock
      tmp <- getElementPtr0 usage (0::Word32, ())
      let p3 = castVarArgs printf :: (Function (Ptr Word8 -> IO Word32))
      _ <- call p3 tmp
      ret (1::Word32)

      defineBasicBlock run
      tmp3 <- getElementPtr argv (1::Word32, ())
      tmp4 <- load tmp3
      nfib <- call atoi tmp4
      call func nfib
      ret (1::Word32)
      
    return $ HelloMod func fib main

main :: IO ()
main = do
    initializeNativeTarget
    mod <- newNamedModule "fib"
    fns <- defineModule mod bldGreet
    greet <- runEngineAccess $ (addModule mod >> generateFunction (mGreet fns))
    writeBitcodeToFile "HelloJIT.bc" mod

    greet 5
    greet 6
    greet 7
    return ()
