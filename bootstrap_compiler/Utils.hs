module Utils(join, strip, replace, indent, dirWalk, 
             showPair, showCommas, showTuple, showRecord,
             sortPairs, eqTuple, eqRecord) where
import Data.List 
import Directory
import Monad hiding (join)

join sep [] = ""
join sep ws = foldr1 (\w s -> w ++ sep ++ s) ws

strip 0 back xs = 
  let len = length xs in
  if back <= len then take (len - back) xs else xs
strip front back xs = strip 0 back (drop front xs)

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] newSub list = list
replace oldSub newSub list = _replace list where
	_replace list@(h:ts) = if isPrefixOf oldSub list
		then newSub ++ _replace (drop len list)
		else h : _replace ts
	_replace [] = []
	len = length oldSub

indent text level = replace "\n" (concat $ replicate level "    ") text

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

showPair (label, value) = label ++ ": " ++ show value
showCommas args = join ", " (map show args)
showTuple valList = "(" ++ (if length valList == 1 
    then show (head valList) ++ "," 
    else showCommas valList) ++ ")"
showRecord valList = "{" ++ join ", " (map showPair valList) ++ "}"

sortPairs :: (Ord a, Eq b) => [(a, b)] -> [(a, b)]
sortPairs = sortBy fieldCompare where fieldCompare (x, _) (y, _) = compare x y

eqTuple :: (Eq a) => [a] -> [a] -> Bool
eqTuple x1 x2 = and $ zipWith (==) x1 x2

eqRecord :: (Ord a, Eq b) => [(a, b)] -> [(a, b)] -> Bool
eqRecord x1 x2 = and $ zipWith (==) (sortPairs x1) (sortPairs x2)
