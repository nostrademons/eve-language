module Main(main) where
import Data.List
import IO
import Monad
import Text.Pandoc

import Utils

filterExtensions ext = notHas ".svn" . notHas "~" . notHas ".swp" . hasExt ext
  where
    notHas ext = filter (not . isSuffixOf ext)
    hasExt ext = filter (isSuffixOf ("." ++ ext))

extractCode :: FilePath -> IO String
extractCode filename = do
    file <- openFile "/Users/jdtang/Documents/programming/eve/eve-language/test/number.evetest" ReadMode
    contents <- hGetContents file
    pandoc <- return $ readMarkdown defaultParserState contents
    return $ concat $ codeBlocks pandoc
  where
    codeBlocks (Pandoc _ blocks) = map codeBlockText blocks
    codeBlockText (CodeBlock _ text) = text ++ "\n"
    codeBlockText _ = ""

main = do
    files <- liftM (filterExtensions "evetest") $ dirWalk "../test"
    text <- mapM extractCode files
    putStrLn $ concatMap (++ "\n") text
