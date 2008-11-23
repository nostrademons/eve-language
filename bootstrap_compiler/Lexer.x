{
module Lexer (lexer) where
import Control.Monad.Error

import Utils
import SourcePos
import Token
import Error

-- Actions should have type SourcePos -> String -> ActionResult

}

$digit = [0-9]	      	        -- digits
$alnum = [A-Za-z]               -- alphanumeric
$delim = [\( \) \[ \] \{ \}]    -- delimiters
$singleQuoteChar = [$printable \r\n] # \'
$doubleQuoteChar = [$printable \r\n] # \"

tokens :-
  <0> "-"       { tokenStr TokKeyword }
  <0> "True"    { tokenStr $ const (TokBool True) }
  <0> "False"   { tokenStr $ const (TokBool False) }
  <0> ","       { tokenStr TokKeyword }
  <0> "."       { tokenStr TokOp }
  <0> ":"       { tokenStr TokKeyword }
  <0> "@"       { tokenStr TokKeyword }
  <0> "?"       { tokenStr TokVar }

  <0> "|"   { tokenStr TokOp } 
  <0> "&"   { tokenStr TokOp } 
  <0> "~"   { tokenStr TokOp } 

  <0> "**"  { tokenStr TokOp } 
  <0> "*"   { tokenStr TokOp } 
  <0> "/"   { tokenStr TokOp } 
  <0> "%"   { tokenStr TokOp } 
  <0> "+"   { tokenStr TokOp } 
  <0> "<"   { tokenStr TokOp } 
  <0> ">"   { tokenStr TokOp } 
  <0> "="   { tokenStr TokOp }
  <0> "<="  { tokenStr TokOp } 
  <0> ">="  { tokenStr TokOp } 
  <0> "=="  { tokenStr TokOp } 
  <0> "!="  { tokenStr TokOp } 
  <0> "->"  { tokenStr TokOp } 
  <0> ".."  { tokenStr TokOp } 

  <0> $delim                        { tokenChar TokDelim }
  <0> $digit+                       { tokenStr $ TokInt . read }

  <0> "'"                                           { begin string1 }
  <string1> ($singleQuoteChar # [\r\n])* "'"        { end 1 $ tokenStr $ TokString . strip 0 1 }
  <string1> "''" ($singleQuoteChar)* "'''"          { end 1 $ tokenStr $ TokString . strip 2 3 }

  <0> \"                                            { begin string2 }
  <string2> ($doubleQuoteChar # [\r\n])* \"         { end 1 $ tokenStr $ TokString . strip 0 1 }
  <string2> \"\" ($doubleQuoteChar)* \"\"\"         { end 1 $ tokenStr $ TokString . strip 2 3 }

  <0> [$alnum _] [$alnum $digit _]*     { tokenStr TokVar }
  <0> ":" [$alnum _] [$alnum $digit _]* { tokenStr $ TokSym . strip 1 0 }

  <0> "//" [^\n]* \n                ;
  <0> "#" [^\n]* \n	                ;
  <0> [$white]+	                    ;

{

-- The input type


type AlexInput = (SourcePos, 	-- current position,
		          Char,		    -- previous char
		          String)	    -- current input string

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

alexScanTokens :: String -> String -> [Either EveError Token]
alexScanTokens filename str = go (alexStartPos filename,'\n',str) 0
  where 
    go inp@(pos, _, str) startCode = case alexScan inp startCode of
        AlexEOF -> []
        AlexError (nextPos, _, (c:_)) -> [Left $ LexError c nextPos]
        AlexError (nextPos, _, []) -> [Left $ LexError '\n' nextPos]
        AlexSkip  inp' len     -> go inp' startCode
        AlexToken inp' len act -> case act pos (take len str) of
            StartState newStartCode -> go inp' newStartCode
            EndState token -> return token : go inp' 0
            ActionToken token -> return token : go inp' startCode

data ActionResult = 
    StartState Int
  | EndState Token
  | ActionToken Token

-- Helper functions to return token actions
begin :: Int -> SourcePos -> String -> ActionResult
begin startCode pos str = StartState startCode

end :: Int -> (SourcePos -> String -> ActionResult) -> SourcePos -> String -> ActionResult
end posminus action pos@(Pos f a l c) str = EndState $ tok
  where ActionToken tok = action (Pos f (a - posminus) l (c - posminus)) str

tokenStr :: (String -> TokenValue) -> SourcePos -> String -> ActionResult
tokenStr constr pos str = ActionToken $ Token (constr str) pos

tokenChar :: (Char -> TokenValue) -> SourcePos -> String -> ActionResult
tokenChar constr pos str = ActionToken $ Token (constr (str !! 0)) pos

extractNum [(num, _)] = num

-----

-- Operators are tokens where a following or preceding newline is ignored,
-- while keywords trigger no special newline handling
operators = ["and", "or", "not", "then", "else", "as"]
keywords = ["if", "import", "export", "def", "class", "cond", "typedef", "None"]

replaceKeywords :: [Token] -> [Token]
replaceKeywords = map changeKeyword 
  where
    changeKeyword (Token (TokVar text) pos)
      | text `elem` operators = Token (TokOp text) pos
      | text `elem` keywords = Token (TokKeyword text) pos
      | otherwise = Token (TokVar text) pos
    changeKeyword nonVar = nonVar


-- Search for newlines.  A newline token is added whenever the line number
-- of the next token is greater than that of the previous token, and:
-- * The previous token is not an operator or keyword
-- * The next token is not an operator
-- * The token is not inside of a matching pair of delimiters.

type DelimState = (Int, Int, Int)

noDelims :: DelimState -> Bool
noDelims (parens, braces, brackets) = parens == 0 && braces == 0 && brackets == 0

hasLineBreak :: Token -> [Token] -> Bool
hasLineBreak (Token (TokOp _) _) _ = False
hasLineBreak _ ((Token (TokOp _) _):_) = False
hasLineBreak (Token _ (Pos _ _ line _)) (Token _ (Pos _ _ nextLine _):rest) 
    = line < nextLine
hasLineBreak _ [] = False

addNewlines :: DelimState -> [Token] -> [Token]
addNewlines delims@(parens, braces, brackets) (tok@(Token (TokDelim c) pos) : rest)
  | c == '(' || c == '{' || c == '[' = startDelim $ addDelim delims
  | noDelims subDelim && hasLineBreak tok rest = 
            tok : (Token TokNewline pos) : addNewlines subDelim rest
  | otherwise = tok : addNewlines subDelim rest
  where 
    startDelim newDelims = (Token (TokDelim c) pos) : addNewlines newDelims rest
    addDelim = case c of
      '(' -> \(parens, braces, brackets) -> (parens + 1, braces, brackets)
      '{' -> \(parens, braces, brackets) -> (parens, braces + 1, brackets)
      '[' -> \(parens, braces, brackets) -> (parens, braces, brackets + 1)
    subDelim = case c of
      ')' -> if parens > 0 then (parens - 1, braces, brackets) else delims
      '}' -> if braces > 0 then (parens, braces - 1, brackets) else delims
      ']' -> if brackets > 0 then (parens, braces, brackets - 1) else delims
addNewlines delims (tok@(Token _ pos) : rest) = if noDelims delims && hasLineBreak tok rest
  then tok : (Token TokNewline pos) : addNewlines delims rest
  else tok : addNewlines delims rest
addNewlines delims [] = []

addIndents :: [Int] -> [Token] -> [Token]
addIndents indentStack [] = 
    replicate (length indentStack) (Token TokDedent $ Pos "endfile" 0 0 0)
addIndents indentStack (tok@(Token TokNewline pos) : []) = [tok]
addIndents indentStack (tok@(Token TokNewline pos) : next : rest)
  | indent > lastIndent = tok : (Token TokIndent pos) : next 
                            : addIndents (indent : indentStack) rest
  | indent < lastIndent = dedents ++ [tok, next] ++ 
                            addIndents (drop numDedents indentStack) rest
  | indent == lastIndent = tok : next : addIndents indentStack rest
  where 
    lastIndent = if null indentStack then 1 else head indentStack
    Token _ nextPos = next
    Pos _ _ _ indent = nextPos
    dedents = zipWith Token (replicate numDedents TokDedent) (repeat pos)
    numDedents = length $ takeWhile (> indent) indentStack
addIndents indentStack (nonNewline : rest) = nonNewline : addIndents indentStack rest

lexer :: String -> String -> Either EveError [Token]
lexer filename input = sequence (alexScanTokens filename input) 
          >>= return . addIndents [] . addNewlines (0, 0, 0) . replaceKeywords

}
