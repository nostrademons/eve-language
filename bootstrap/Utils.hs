module Utils (strip, stripTo, contains, multiLookup, dirWalk, openTestFile) where
import Monad hiding (join)
import IO
import Directory
import Data.List

strip 0 back xs = 
  let len = length xs in
  if back <= len then take (len - back) xs else xs
strip front back xs = strip 0 back (drop front xs)

-- For some reason, isInfixOf isn't in my version of GHC.
contains [] needle = False
contains haystack@(x:xs) needle = 
    needle `isPrefixOf` haystack || xs `contains` needle

stripTo haystack needle = 
    drop (length (dropWhile (not . isSuffixOf needle) (inits haystack) !! 0)) haystack

multiLookup :: (Eq a) => a -> [(a, b)] -> [b]
multiLookup key [] = []
multiLookup key ((k, v) : rest) = (if k == key then (v:) else id) $ multiLookup key rest

dirWalk :: FilePath -> IO [FilePath]
dirWalk dir = let
    isValid name = not $ ("/." `isSuffixOf` name) || ("/.." `isSuffixOf` name)
    listDirs = filterM doesDirectoryExist
    listFiles = filterM doesFileExist
    joinName base path = base ++ "/" ++ path
  in do
    noDots <- getDirectoryContents dir 
          >>= filterM (return . isValid) . map (joinName dir)
    subDirFiles <- listDirs noDots >>= mapM dirWalk >>= return . concat
    files <- listFiles noDots
    return $ files ++ subDirFiles

openTestFile filename = openFile filename ReadMode 
                    >>= hGetContents >>= return . condenseEscapes . lines

condenseEscapes [] = []
condenseEscapes [x] = [x]
condenseEscapes (x:xs) = if "\\" `isSuffixOf` x 
    then (take (length x - 1) x ++ "\n" ++ sameLine) : restOfLines
    else x : sameLine : restOfLines
  where (sameLine : restOfLines) = condenseEscapes xs
