module Utils(join, replace, indent, 
            showPair, showTuple, showRecord, sortPairs, eqTuple, eqRecord) where

join sep [] = ""
join sep ws = foldr1 (\w s -> w ++ sep ++ s) ws

replace::(Eq a) => [a] -> [a] -> [a] -> [a]
replace [] newSub list = join newSub list
replace oldSub newSub list = _replace list where
	_replace list@(h:ts) = if isPrefixOf oldSub list
		then newSub ++ _replace (drop len list)
		else h : _replace ts
	_replace [] = []
	len = length oldSub

indent text level = replace "\n" (concat $ replicate "    " level) tex

showPair (label, value) = "'" ++ label ++ "': " ++ show value
showTuple valList = "[" ++ join ", " (map show exprList) ++ "]"
showRecord valList = "{" ++ join ", " (map showPair pairList) ++ "}"

sortPairs = sortBy fieldCompare where fieldCompare (x, _) (y, _) = compare x y
eqTuple x1 x2 = and $ zipWith (==) x1 x2
eqRecord x1 x2 = and $ zipWith (==) (sortPairs x1) (sortPairs x2)
