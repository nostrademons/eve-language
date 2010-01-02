module Main(main) where
import Data.Word
import IO
import System
import System.Cmd
import LLVM.Core

compilePlaceholder :: CodeGenModule ()
compilePlaceholder = do
  printf <- newNamedFunction ExternalLinkage "printf"
      :: TFunction (Ptr Word8 -> IO Word32)
  msg <- createStringNul "Hello, Eve!\n"
  main <- newNamedFunction ExternalLinkage "main"
      :: TFunction (Word32 -> Ptr (Ptr Word8) -> IO Word32)
  defineFunction main $ \argc argv -> do
    tmp <- getElementPtr0 msg (0::Word32, ())
    call printf tmp
    ret (1::Word32)

writeCompiledFile moduleName = do
  mod <- newNamedModule moduleName
  defineModule mod compilePlaceholder
  let bcFile = moduleName ++ ".bc"
  let sFile = moduleName ++ ".s"
  writeBitcodeToFile bcFile mod
  rawSystem "llc" [bcFile]
  rawSystem "gcc" [sFile, "-o", moduleName]

main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: eve <file to compile>"
    else do
      let filename = args !! 0
      inFile <- openFile (filename ++ ".evetest") ReadMode
      text <- hGetContents inFile
      putStrLn text
      writeCompiledFile filename
      return ()
