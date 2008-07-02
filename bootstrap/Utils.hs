module Utils (strip, multiLookup) where

strip 0 back xs = 
  let len = length xs in
  if back <= len then take (len - back) xs else xs
strip front back xs = strip 0 back (drop front xs)

multiLookup :: (Eq a) => a -> [(a, b)] -> [b]
multiLookup key [] = []
multiLookup key ((k, v) : rest) = (if k == key then (v:) else id) $ multiLookup key rest
