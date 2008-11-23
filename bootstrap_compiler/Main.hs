module Main(main) where
import System
import IO
import Control.Monad.Error

import Lexer
import Parser
import Error

main = do
    [filename] <- getArgs
    text <- openFile filename ReadMode >>= hGetContents
    Right parsedText <- return $ (lexer filename text >>= parseFile >>= return . unlines . map show)
                    `catchError` (return . show)
    putStrLn parsedText
