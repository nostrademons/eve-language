module Main(main) where
import Data.List

import Utils

filterExtensions ext = notHas ".svn" . notHas "~" . notHas ".swp" . hasExt ext
  where
    notHas ext = filter (not . isSuffixOf ext)
    hasExt ext = filter (isSuffixOf ("." ++ ext))

main = dirWalk "../test" >>=
       putStrLn . concatMap (++ "\n") . filterExtensions "evetest"
