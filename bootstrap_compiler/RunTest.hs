module Main(main) where
import Data.List
import Monad
import Text.Pandoc

import Utils

filterExtensions ext = notHas ".svn" . notHas "~" . notHas ".swp" . hasExt ext
  where
    notHas ext = filter (not . isSuffixOf ext)
    hasExt ext = filter (isSuffixOf ("." ++ ext))

main = do
  files <- liftM (filterExtensions "evetest") $ dirWalk "../test"
  putStrLn $ concatMap (++ "\n") files
