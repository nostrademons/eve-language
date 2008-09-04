module Main(main) where
import IO
import System.Environment hiding (getEnv)
import Data
import Utils
import Lexer
import Parser

printLex filename = openFile filename ReadMode >>= hGetContents >>= lexParse >>= showResult
  where 
    lexParse text = runEveM (lexer filename text >>= parseFile) []
    showResult (Left err) = putStrLn $ show err
    showResult (Right (parseTree, _)) = putStrLn $ join "\n" $ map (showFileLine 0) parseTree
    showFileLine i (line@Def {}, pos) = showLine i line (def_lines line) pos
    showFileLine i (line@Class {}, pos) = showLine i line (snd . class_doc_lines $ line) pos
    showFileLine i (line, pos) = showLine i line [] pos
    showLine i obj lines pos = 
        indent i ++ show obj ++ " @ " ++ show pos ++ "\n" ++ (join "\n" $ map (showFileLine (i + 1)) lines)
    indent i = replicate (i * 2) ' '
main = getArgs >>= printLex . (!! 0)
