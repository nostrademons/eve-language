module Main(main) where
import Data.List
import IO
import Monad
import Text.Pandoc

import Utils

data Fixture = Fixture String
data Expect = Expect String String
data CodeChunk =
    CodeTest [Expect]
  | CodeChunk Fixture

filterExtensions ext = notHas ".svn" . notHas "~" . notHas ".swp" . hasExt ext
  where
    notHas ext = filter (not . isSuffixOf ext)
    hasExt ext = filter (isSuffixOf ("." ++ ext))

parseCode :: String -> [CodeChunk]
parseCode = map parseBlock . codeBlocks . readMarkdown defaultParserState
  where
    codeBlocks (Pandoc _ blocks) = filter isCodeBlock blocks
    isCodeBlock (CodeBlock _ _) = True
    isCodeBlock _ = False
    isDocTest = isPrefixOf ">>> "
    parseDocTests (line:lines) =
        Expect cleanedInput (if null output then "" else last output) :
            parseDocTests remainder
      where
        cleanedInput = drop (length ">>> ") line
        (output, remainder) = break isDocTest lines
    parseDocTests [] = []
    parseBlock (CodeBlock _ text) =
        if isDocTest text
          then CodeTest (parseDocTests $ lines text)
          else CodeChunk (Fixture text)

generateCode :: [CodeChunk] -> String
generateCode chunks = concatMap generateCodeFor chunks
  where
    generateCodeFor (CodeChunk (Fixture text)) = text
    generateCodeFor (CodeTest tests) = concatMap generateTestCode tests
    generateTestCode (Expect input _) =
        "print(\"" ++ input ++ "\")\nprint(" ++ input ++ ")\n"

outputCode :: FilePath -> IO String
outputCode filename = do
    file <- openFile filename ReadMode
    contents <- hGetContents file
    return . generateCode . parseCode $ contents
  where

main = do
    files <- liftM (filterExtensions "evetest") $ dirWalk "../test"
    text <- mapM outputCode files
    putStrLn $ concatMap (++ "\n") text
