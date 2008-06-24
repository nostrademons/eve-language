module Utils (strip) where

strip 0 back xs = 
  let len = length xs in
  if back < len then take (len - back) xs else xs
strip front back xs = strip 0 back (drop front xs)
