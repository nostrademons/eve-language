module Utils (strip, multiLookup, openTestFile) where
import IO
import Data.List

strip 0 back xs = 
  let len = length xs in
  if back <= len then take (len - back) xs else xs
strip front back xs = strip 0 back (drop front xs)

multiLookup :: (Eq a) => a -> [(a, b)] -> [b]
multiLookup key [] = []
multiLookup key ((k, v) : rest) = (if k == key then (v:) else id) $ multiLookup key rest

openTestFile filename = openFile filename ReadMode 
                    >>= hGetContents >>= return . condenseEscapes . lines

condenseEscapes [] = []
condenseEscapes [x] = [x]
condenseEscapes (x:xs) = if "\\" `isSuffixOf` x 
    then (take (length x - 1) x ++ "\n" ++ sameLine) : restOfLines
    else x : sameLine : restOfLines
  where (sameLine : restOfLines) = condenseEscapes xs
