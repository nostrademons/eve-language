{
module Lexer (lexer, AlexPosn, showTok) where
import Control.Monad.Error
import Data     
import Utils

-- Actions should have type AlexPosn -> String -> EveToken
}

$digit = [0-9]			-- digits
$alnum = [A-Za-z]               -- alphanumeric
$oper = [\+\*\/&=\<\>\%\!\^]    -- operators (plus -, which needs to be support for "3 + -2")
$delim = [\( \) \[ \] \{ \}]    -- delimiters

tokens :-
  "-"                           { tokenStr TokKeyword }
  "->"                          { tokenStr TokOp }
  "True"                        { tokenStr $ const (TokBool True) }
  "False"                       { tokenStr $ const (TokBool False) }
  "|"                           { tokenStr TokKeyword }
  ","                           { tokenStr TokKeyword }
  ":"                           { tokenStr TokKeyword }
  "."                           { tokenStr TokOp }
  "?"                           { tokenStr TokVar }
  $delim                        { tokenChar TokDelim }
  $digit+                       { tokenStr $ TokInt . read }

  "'" ($printable # [\' \n])* "'"   { tokenStr $ TokString . strip 1 1 }
  \" ($printable # [\" \n])* \"     { tokenStr $ TokString . strip 1 1 }

  [$alnum _] [$alnum $digit _]*     { tokenStr TokVar }
  "\" [$alnum $digit $oper \-_]+    { tokenStr TokVar }
  $oper{1,2} 	                    { tokenStr TokOp }
  "//" [^\n]* \n                ;
  "#" [^\n]* \n	                ;
  [$white]+	                    ;

{

-- The input type


type AlexInput = (AlexPosn, 	-- current position,
		  Char,		-- previous char
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
				Just (c, (p', c, s))

-- ---------------------------------------------------------------------------
-- 
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

alexScanTokens :: (MonadError EveError m) => String -> [m (AlexPosn, EveToken)]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
	  case alexScan inp 0 of
		AlexEOF -> []
		AlexError (posn, _, (c:_)) -> [throwError $ LexError c posn]
		AlexSkip  inp' len     -> go inp'
		AlexToken inp' len act -> return (act pos (take len str)) : go inp'


-----

-- Replace appropriate TokVar instances with keywords
operators = ["and", "or", "not", "then", "else"]
keywords = ["if", "import", "export", "def"]

replaceKeywords :: [(AlexPosn, EveToken)] -> [(AlexPosn, EveToken)]
replaceKeywords = map changeKeyword 
  where
    changeKeyword (pos, TokVar text)
      | text `elem` operators = (pos, TokOp text)
      | text `elem` keywords = (pos, TokKeyword text)
      | otherwise = (pos, TokVar text)
    changeKeyword nonVar = nonVar


-- Search for newlines.  A newline token is added whenever the line number
-- of the next token is greater than that of the previous token, and:
-- * The previous token is not an operator or keyword
-- * The next token is not an operator
-- * The token is not inside of a matching pair of delimiters.

type DelimState = (Int, Int, Int)

noDelims :: DelimState -> Bool
noDelims (parens, braces, brackets) = parens == 0 && braces == 0 && brackets == 0

hasLineBreak :: (AlexPosn, EveToken) -> [(AlexPosn, EveToken)] -> Bool
hasLineBreak (AlexPn _ line _, _) ((AlexPn _ nextLine _, _):rest) = line < nextLine
hasLineBreak _ [] = False

startDelim incr c state pos rest = (pos, TokDelim c) : addNewlines (incr state) rest

addNewlines :: DelimState -> [(AlexPosn, EveToken)] -> [(AlexPosn, EveToken)]
addNewlines delims@(parens, braces, brackets) (tok@(pos, TokDelim c) : rest)
  | c == '(' || c == '{' || c == '[' = startDelim addDelim c delims pos rest
  | noDelims newDelims && hasLineBreak tok rest = 
            tok : (pos, TokNewline) : addNewlines newDelims rest
  | otherwise = tok : addNewlines newDelims rest
  where 
    addDelim = case c of
      '(' -> \(parens, braces, brackets) -> (parens + 1, braces, brackets)
      '{' -> \(parens, braces, brackets) -> (parens, braces + 1, brackets)
      '[' -> \(parens, braces, brackets) -> (parens, braces, brackets + 1)
    newDelims = case c of
      ')' -> if parens > 0 then (parens - 1, braces, brackets) else delims
      '}' -> if braces > 0 then (parens, braces - 1, brackets) else delims
      ']' -> if brackets > 0 then (parens, braces, brackets - 1) else delims
addNewlines delims (tok@(pos, TokOp _) : rest) = tok : addNewlines delims rest
addNewlines delims (tok@(pos, TokKeyword _) : rest) = tok : addNewlines delims rest
addNewlines delims (tok : op@(pos, TokOp _) : rest) = tok : op : addNewlines delims rest
addNewlines delims (tok@(pos, _) : rest) = if noDelims delims && hasLineBreak tok rest
  then tok : (pos, TokNewline) : addNewlines delims rest
  else tok : addNewlines delims rest
addNewlines delims [] = []

addIndents :: [Int] -> [(AlexPosn, EveToken)] -> [(AlexPosn, EveToken)]
addIndents _ [] = []
addIndents indentStack (tok@(pos, TokNewline) : []) = [tok]
addIndents indentStack (tok@(pos, TokNewline) : next : rest)
  | indent > lastIndent = tok : (pos, TokIndent) : addIndents (indent : indentStack) rest
  | indent < lastIndent = [tok] ++ dedents ++ [next] ++ addIndents newStack rest
  | indent == lastIndent = tok : next : addIndents indentStack rest
  where 
    lastIndent = if null indentStack then 1 else head indentStack
    (nextPos, _) = next
    AlexPn _ _ indent = nextPos
    dedents = zip (repeat nextPos) (replicate numDedents TokDedent)
    numDedents = length $ takeWhile (> indent) indentStack
    newStack = drop numDedents indentStack
addIndents indentStack (nonNewline : rest) = nonNewline : addIndents indentStack rest

lexer :: (MonadError EveError m) => String -> m [(AlexPosn, EveToken)]
lexer input = sequence (alexScanTokens input) 
          >>= return . addIndents [] . addNewlines (0, 0, 0) . replaceKeywords

showTok (_, tok) = show tok

-- Helper functions to return token actions
tokenStr constr posn str = (posn, constr str)
tokenChar constr posn str = (posn, constr (str !! 0))
extractNum [(num, _)] = num
}
