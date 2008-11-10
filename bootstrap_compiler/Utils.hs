module Utils(join, replace, indent) where

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
