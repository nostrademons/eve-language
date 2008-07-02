module Main(main) where
import IO
import System.Environment hiding (getEnv)
import Data
import Utils
import Lexer

printLex filename = openFile filename ReadMode >>= hGetContents >>= 
    flip runEveM [] . lexer >>= showResult
  where 
    showResult (Left err) = putStrLn $ show err
    showResult (Right (tokenList, _)) = putStrLn . show $ map showTok tokenList
main = getArgs >>= printLex . (!! 0)
