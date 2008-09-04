{
module Lexer (lexer, SourcePos, showTok) where
import Control.Monad.Error
import Data     
import Utils

-- Actions should have type SourcePos -> String -> ActionResult

}

$digit = [0-9]	      	        -- digits
$alnum = [A-Za-z]               -- alphanumeric
$oper = [\+\/\|\&\~=\<\>\%\!\^]  -- operators (plus -, which needs to be support for "3 + -2")
$delim = [\( \) \[ \] \{ \}]    -- delimiters
$singleQuoteChar = [$printable \r\n] # \'
$doubleQuoteChar = [$printable \r\n] # \"

tokens :-
  <0> "-"                           { tokenStr TokKeyword }
  <0> "->"                          { tokenStr TokOp }
  <0> "True"                        { tokenStr $ const (TokBool True) }
  <0> "False"                       { tokenStr $ const (TokBool False) }
  <0> ","                           { tokenStr TokKeyword }
  <0> ":"                           { tokenStr TokKeyword }
  <0> "@"                           { tokenStr TokKeyword }
  <0> "."                           { tokenStr TokOp }
  <0> ".."                          { tokenStr TokOp }
  <0> "?"                           { tokenStr TokVar }
  <0> "*"                           { tokenStr TokOp }
  <0> $delim                        { tokenChar TokDelim }
  <0> $digit+                       { tokenStr $ TokInt . read }

  <0> "'"                                           { begin string1 }
  <string1> ($singleQuoteChar # [\r\n])* "'"        { end 1 $ tokenStr $ TokString . strip 0 1 }
  <string1> "''" ($singleQuoteChar)* "'''"          { end 1 $ tokenStr $ TokString . strip 2 3 }

  <0> \"                                            { begin string2 }
  <string2> ($doubleQuoteChar # [\r\n])* \"         { end 1 $ tokenStr $ TokString . strip 0 1 }
  <string2> \"\" ($doubleQuoteChar)* \"\"\"         { end 1 $ tokenStr $ TokString . strip 2 3 }

  <0> [$alnum _] [$alnum $digit _]*     { tokenStr TokVar }
  <0> "\" [$alnum $digit $oper \-_]+    { tokenStr TokVar }
  <0> ":" [$alnum _] [$alnum $digit _]* { tokenStr $ TokSym . strip 1 0 }
  <0> $oper{1,2} 	                    { tokenStr TokOp }
  <0> "//" [^\n]* \n                ;
  <0> "#" [^\n]* \n	                ;
  <0> [$white]+	                    ;

{

data ActionResult = 
    StartState Int
  | EndState (EveToken, SourcePos)
  | Token (EveToken, SourcePos)

-- The input type


type AlexInput = (SourcePos, 	-- current position,
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

alexStartPos :: String -> SourcePos
alexStartPos filename = Pos filename 0 1 1

alexMove :: SourcePos -> Char -> SourcePos
alexMove (Pos f a l c) '\t' = Pos f (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pos f a l c) '\n' = Pos f (a+1) (l+1)   1
alexMove (Pos f a l c) _    = Pos f (a+1)  l     (c+1)

alexScanTokens :: String -> String -> [EveM (EveToken, SourcePos)]
alexScanTokens filename str = go (alexStartPos filename,'\n',str) 0
  where 
    go inp@(pos, _, str) startCode = case alexScan inp startCode of
        AlexEOF -> []
        AlexError (nextPos, _, (c:_)) -> [withPos nextPos $ throwEveError $ LexError c]
        AlexError (nextPos, _, []) -> [withPos nextPos $ throwEveError $ LexError '\n']
        AlexSkip  inp' len     -> go inp' startCode
        AlexToken inp' len act -> case act pos (take len str) of
            StartState newStartCode -> go inp' newStartCode
            EndState token -> return token : go inp' 0
            Token token -> return token : go inp' startCode

-- Helper functions to return token actions
begin startCode pos str = StartState startCode
end posminus action pos@(Pos f a l c) str = 
            EndState $ (token, Pos f (a - posminus) l (c - posminus))
  where Token (token, _) = action pos str
tokenStr constr pos str = Token (constr str, pos)
tokenChar constr pos str = Token (constr (str !! 0), pos)
extractNum [(num, _)] = num

-----

-- Operators are tokens where a following or preceding newline is ignored,
-- while keywords trigger no special newline handling
operators = ["and", "or", "not", "then", "else", "as"]
keywords = ["if", "import", "export", "def", "class", "cond", "typedef"]

replaceKeywords :: [(EveToken, SourcePos)] -> [(EveToken, SourcePos)]
replaceKeywords = map changeKeyword 
  where
    changeKeyword (TokVar text, pos)
      | text `elem` operators = (TokOp text, pos)
      | text `elem` keywords = (TokKeyword text, pos)
      | otherwise = (TokVar text, pos)
    changeKeyword nonVar = nonVar


-- Search for newlines.  A newline token is added whenever the line number
-- of the next token is greater than that of the previous token, and:
-- * The previous token is not an operator or keyword
-- * The next token is not an operator
-- * The token is not inside of a matching pair of delimiters.

type DelimState = (Int, Int, Int)

noDelims :: DelimState -> Bool
noDelims (parens, braces, brackets) = parens == 0 && braces == 0 && brackets == 0

hasLineBreak :: (EveToken, SourcePos) -> [(EveToken, SourcePos)] -> Bool
hasLineBreak (TokOp _, _) _ = False
hasLineBreak _ ((TokOp _, _):_) = False
hasLineBreak (_, Pos _ _ line _) ((_, Pos _ _ nextLine _):rest) = line < nextLine
hasLineBreak _ [] = False

addNewlines :: DelimState -> [(EveToken, SourcePos)] -> [(EveToken, SourcePos)]
addNewlines delims@(parens, braces, brackets) (tok@(TokDelim c, pos) : rest)
  | c == '(' || c == '{' || c == '[' = startDelim $ addDelim delims
  | noDelims subDelim && hasLineBreak tok rest = 
            tok : (TokNewline, pos) : addNewlines subDelim rest
  | otherwise = tok : addNewlines subDelim rest
  where 
    startDelim newDelims = (TokDelim c, pos) : addNewlines newDelims rest
    addDelim = case c of
      '(' -> \(parens, braces, brackets) -> (parens + 1, braces, brackets)
      '{' -> \(parens, braces, brackets) -> (parens, braces + 1, brackets)
      '[' -> \(parens, braces, brackets) -> (parens, braces, brackets + 1)
    subDelim = case c of
      ')' -> if parens > 0 then (parens - 1, braces, brackets) else delims
      '}' -> if braces > 0 then (parens, braces - 1, brackets) else delims
      ']' -> if brackets > 0 then (parens, braces, brackets - 1) else delims
addNewlines delims (tok@(_, pos) : rest) = if noDelims delims && hasLineBreak tok rest
  then tok : (TokNewline, pos) : addNewlines delims rest
  else tok : addNewlines delims rest
addNewlines delims [] = []

addIndents :: [Int] -> [(EveToken, SourcePos)] -> [(EveToken, SourcePos)]
addIndents indentStack [] = replicate (length indentStack) (TokDedent, Pos "endfile" 0 0 0)
addIndents indentStack (tok@(TokNewline, pos) : []) = [tok]
addIndents indentStack (tok@(TokNewline, pos) : next : rest)
  | indent > lastIndent = tok : (TokIndent, pos) : next : addIndents (indent : indentStack) rest
  | indent < lastIndent = dedents ++ [tok, next] ++ addIndents (drop numDedents indentStack) rest
  | indent == lastIndent = tok : next : addIndents indentStack rest
  where 
    lastIndent = if null indentStack then 1 else head indentStack
    (_, nextPos) = next
    Pos _ _ _ indent = nextPos
    dedents = zip (replicate numDedents TokDedent) (repeat pos)
    numDedents = length $ takeWhile (> indent) indentStack
addIndents indentStack (nonNewline : rest) = nonNewline : addIndents indentStack rest

lexer :: String -> String -> EveM [(EveToken, SourcePos)]
lexer filename input = sequence (alexScanTokens filename input) 
          >>= return . addIndents [] . addNewlines (0, 0, 0) . replaceKeywords

showTok (tok, _) = show tok

}
