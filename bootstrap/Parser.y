{
module Parser(parseRepl, parseFile) where
import Control.Monad.Error hiding (join)
import Lexer
import Data
import Primitives
}

%name eveFile File
%name replLine ReplLine
%monad { EveM } 
%tokentype { (EveToken, SourcePos) }

%token
INT   { (TokInt $$, _) }
BOOL  { (TokBool $$, _) }
STR   { (TokString $$, _) }
SYM   { (TokSym $$, _) }
VAR   { (TokVar _, _) }
EOL   { (TokNewline, _) }
INDENT { (TokIndent, _) }
DEDENT { (TokDedent, _) }
'**'  { (TokOp "**", _) }
'*'   { (TokOp "*", _) }
'/'   { (TokOp "/", _) }
'%'   { (TokOp "%", _) }
'+'   { (TokOp "+", _) }
'-'   { (TokKeyword "-", _) }
'<'   { (TokOp "<", _) }
'>'   { (TokOp ">", _) }
'<='  { (TokOp "<=", _) }
'>='  { (TokOp ">=", _) }
'=='  { (TokOp "==", _) }
'!='  { (TokOp "!=", _) }
'='   { (TokOp "=", _)  }
'->'  { (TokOp "->", _) }
'..'  { (TokOp "..", _) }
'and' { (TokOp "and", _) }
'or'  { (TokOp "or", _) }
'not' { (TokOp "not", _) }
'def' { (TokKeyword "def", _) }
'class'{(TokKeyword "class", _) }
'if'  { (TokKeyword "if", _) }
'then'{ (TokOp "then", _) }
'else'{ (TokOp "else", _) }
'cond'{ (TokKeyword "cond", _) }
'as'  { (TokOp "as", _) }
'typedef'   { (TokKeyword "typedef", _) }
','   { (TokKeyword ",", _) }
'.'   { (TokOp ".", _) }
'|'   { (TokOp "|", _) }
'&'   { (TokOp "&", _) }
'~'   { (TokOp "~", _) }
':'   { (TokKeyword ":", _) }
'@'   { (TokKeyword "@", _) }
'('   { (TokDelim '(', _) }
')'   { (TokDelim ')', _) }
'{'   { (TokDelim '{', _) }
'}'   { (TokDelim '}', _) }
'['   { (TokDelim '[', _) }
']'   { (TokDelim ']', _) }
'import' { (TokKeyword "import", _) }
'export' { (TokKeyword "export", _) }

%left '&' '~'
%left '|'
%left 'and' 'or'
%nonassoc 'not'
%nonassoc '!=' '==' '>=' '<=' '>' '<'
%nonassoc '..'
%left '+' '-'
%left '*' '/' '%'
%left '**'
%left NEG
%left '->'

%%

File : FileLine { [$1] }
     | File EOL FileLine { $3 : $1 }

FileLine : SequenceUnpack               { $1 }
         | 'import' DottedIdent         { (Import (reverse $2), pos $1) }
         | 'export' VarList             { (Export (reverse $2), pos $1) }
         | 'typedef' VAR ':' TypeExpr   { (TypeDef (extractVar $2) $4, pos $1) }
         | DefDecl                      { $1 }
         | ClassDecl                    { $1 }

DefDecl     : 'def' VAR '(' VarArgList ')' TypeDecl ':' DefBody
    { (makeDef (extractVar $2) $4 $6 $8, pos $1) }

ClassDecl   : 'class' VAR SubClassDecl ':' DefBody   { (Class (extractVar $2) $3 $5, pos $1) }

SubClassDecl    : {- Empty -}   { Nothing }
                | '(' VAR ')'   { Just (extractVar $2) }

DocString   : {- Empty -}       { "" }
            | STR EOL           { $1 }

DefaultDecl : {- Empty -}           { Nothing }
            | '=' Operand           { Just $2 }

TypeDecl    : {- Empty -}       { Nothing }
            | 'as' TypeExpr     { Just $2 }

TypeExpr    : VAR                               { TPrim (extractVar $1) }
            | Literal                           { TLiteral $1 }
            | '[' TypeList ']'                  { TTuple (reverse $2) }
            | '{' LabeledTypeList '}'           { TRecord (reverse $2) }
            | '(' TypeList '->' TypeExpr ')'    { TFunc (reverse $2) $4 }

TypeList    : TypeExpr                  { [$1] }
            | TypeList ',' TypeExpr     { $3 : $1 }

LabeledTypePair     : Label ':' TypeExpr    { ($1, $3) }

LabeledTypeList     : LabeledTypePair                       { [$1] }
                    | LabeledTypeList ',' LabeledTypePair   { $3 : $1 }

DefBody : Expr                                      { ("", [(NakedExpr $1, pos $1)]) }
        | EOL INDENT DocString DefLineList DEDENT   { ($3, reverse $4) }

DefLine : Expr                  { (NakedExpr $1, pos $1) }
        | MultiLineExpr         { (NakedExpr $1, pos $1) }
        | SequenceUnpack        { $1 }
        | DefDecl               { $1 }
        | ClassDecl             { $1 }

DefLineList     : DefLine                   { [$1] }
                | DefLineList EOL DefLine { $3 : $1 }

ReplLine : Expr                 { Expr $1 }
         | 'import' DottedIdent { ReplImport (reverse $2) }
         | VAR '=' Expr         { Assignment (extractVar $1) $3 }

MultiLineExpr   : 'cond' ':' EOL INDENT CondClauseList DEDENT   { (Cond (reverse $5), pos $1) }

SequenceUnpack : VAR '=' Expr          { (Binding (Right $ extractVar $1) $3, pos $2) }
               | VarList '=' Expr      { (Binding (Left $1) $3, pos $2) }

CondClause : Expr ':' Expr      { ($1, $3) }

CondClauseList : CondClause                     { [$1] }
                | CondClauseList EOL CondClause { $3 : $1 }

Expr : Operand             { $1 }
     | '-' Expr %prec NEG  { funcall (pos $1) "sub" [(Literal (makeInt 0), pos $1), $2] }
     | Expr '**' Expr { funcall (pos $2) "pow" [$1, $3] }
     | Expr '*' Expr { funcall (pos $2) "mul" [$1, $3] }
     | Expr '/' Expr { funcall (pos $2) "div" [$1, $3] }
     | Expr '%' Expr { funcall (pos $2) "mod" [$1, $3] }
     | Expr '+' Expr { funcall (pos $2) "add" [$1, $3] }
     | Expr '-' Expr { funcall (pos $2) "sub" [$1, $3] }
     | Expr '..' Expr { funcall (pos $2) "Range" [$1, $3] }
     | Expr '..' Expr { funcall (pos $2) "Range" [$1, $3] }
     | Expr '==' Expr { funcall (pos $2) "eq" [$1, $3] }
     | Expr '!=' Expr { funcall (pos $2) "ne" [$1, $3] }
     | Expr '>' Expr  { funcall (pos $2) "gt" [$1, $3] }
     | Expr '<' Expr  { funcall (pos $2) "lt" [$1, $3] }
     | Expr '>=' Expr { funcall (pos $2) "ge" [$1, $3] }
     | Expr '<=' Expr { funcall (pos $2) "le" [$1, $3] }
     | Expr 'and' Expr  { funcall (pos $2) "and_" [$1, $3] }
     | Expr 'or' Expr   { funcall (pos $2) "or_" [$1, $3] }
     | 'not' Expr       { funcall (pos $1) "not_" [$2] }
     | Expr '|' Expr    { funcall (pos $2) "extend" [$1, $3] }
     | Expr '&' Expr    { funcall (pos $2) "restrict" [$1, $3] }
     | Expr '~' Expr    { funcall (pos $2) "exclude" [$1, $3] }
     | Expr '->' Expr   { (Funcall $3 [$1], pos $2) }
     | 'if' Expr 'then' Expr 'else' Expr
       { (Cond [($2, $4), ((Literal (makeBool True), pos $1), $6)], pos $1) }

{- Faking the position on Literal so we don't need to plumb the position back 
    up from them; errors never happen on literals anyway. -}
Operand     : Literal                      { (Literal $1, defaultPos) }
            | VAR                          { (Variable $ extractVar $1, pos $1) }
            | '[' ExprList ']'             { (TupleLiteral (reverse $2), pos $1) }
            | '{' LabeledList '}'          { (RecordLiteral (reverse $2), pos $1) }
            | '(' Expr ')'                 { $2 }
            | Operand '[' ']'              { methodcall (pos $2) "get" $1 [] }
            | Operand '[' Expr ']'         { methodcall (pos $2) "get" $1 [$3] }
            | Operand '(' ExprList ')'        { (Funcall $1 (reverse $3), pos $2) }
            | Operand '(' ')'                 { (Funcall $1 [], pos $2) }
            | Operand '(' '*' Operand ')'     { funcall (pos $2) "apply" [$1, $4] }
            | Operand '(' ExprList ',' '*' Operand ')' 
                { funcall (pos $2) "apply" [$1, funcall (pos $2) "add" 
                                            [(TupleLiteral (reverse $3), pos $2), $6]] }
            | Operand '.' VAR                 
                { funcall (pos $2) "attr" [$1, (Literal . makeString . extractVar $ $3, pos $2)] }
            | '{' '|' VarArgList '|' Expr '}' { makeLambda (pos $1) $3 $5 }
            | Operand 'as' TypeExpr        { (TypeCheck ($1, $3) $1, pos $2) }

Literal     : INT                          { makeInt $1 }
            | BOOL                         { makeBool $1 }
            | STR                          { makeString $1 }
            | SYM                          { makeSymbol $1 }
            | '[' ']'                      { makeTuple [] }

Label       : STR                      { $1 }
            | VAR                      { extractVar $1 }

DottedIdent : VAR { [extractVar $1] }
            | DottedIdent '.' VAR      { extractVar $3 : $1 }
LabeledPair : Label ':' Expr           { ($1, $3) }

VarList     : VAR                      { [extractVar $1] }
            | VarList ',' VAR          { extractVar $3 : $1 }
ArgList     : {- empty -}                          { [] }
            | VAR DefaultDecl TypeDecl             { [(extractVar $1, $2, $3)] }
            | ArgList ',' VAR DefaultDecl TypeDecl { (extractVar $3, $4, $5) : $1 }
VarArgList  : ArgList                  { ($1, Nothing) }
            | '*' VAR                  { ([], Just . extractVar $ $2) }
            | ArgList ',' '*' VAR      { ($1, Just . extractVar $ $4) }
ExprList    : Expr                     { [$1] }
            | ExprList ',' Expr        { $3 : $1 }
LabeledList : LabeledPair                   { [$1] }
            | LabeledList ',' LabeledPair   { $3 : $1 }

{

parseFile :: [(EveToken, SourcePos)] -> EveM [EveFileLine]
parseFile input = eveFile input >>= return . reverse . map replaceFilePartials 

parseRepl :: [(EveToken, SourcePos)] -> EveM EveReplLine
parseRepl input = replLine input >>= return . replaceReplPartials

pos :: (a, SourcePos) -> SourcePos
pos = snd

extractVar :: (EveToken, SourcePos) -> String
extractVar (TokVar val, _) = val

makeDef name (rawArgs, varargs) typeDecl rawBody = 
    Def name (ArgExpr args defaults varargs) docString typeDecl lines newBody 
  where
    (args, defaults, newBody) = parseArgList rawArgs body 
    (lines, docString, body) = findLastExpr rawBody

replaceFilePartials :: EveFileLine -> EveFileLine
replaceFilePartials (Binding var expr, pos) = (Binding var $ replacePartials expr, pos)
replaceFilePartials (Def name argData docstring typeDecl defines body, pos) =
    (Def name argData docstring typeDecl (map replaceFilePartials defines) (replacePartials body), pos)
replaceFilePartials (Class name superclass (docstring, lines), pos) =
    (Class name superclass (docstring, map replaceFilePartials lines), pos)
replaceFilePartials line = line

replaceReplPartials :: EveReplLine -> EveReplLine
replaceReplPartials (Expr expr) = Expr $ replacePartials expr
replaceReplPartials (Assignment var expr) = Assignment var $ replacePartials expr
replaceReplPartials line = line

-- Also TODO: Expand the legal positions so that eg. if statements can also have partials
replacePartials :: EveExpr -> EveExpr
replacePartials expr@(Literal val, pos) = expr
replacePartials (TupleLiteral args, pos) = maybeLambda pos (\vals -> (TupleLiteral vals, pos)) args
replacePartials (RecordLiteral pairs, pos) = maybeLambda pos reconstruct $ snd $ unzip pairs
  where
    labels = fst $ unzip pairs
    reconstruct vals = (RecordLiteral $ zip labels vals, pos)
replacePartials (Variable var, pos) = (Variable var, pos)
replacePartials (Cond condList, pos) = (Cond $ map handleClause condList, pos)
  where handleClause (pred, action) = (replacePartials pred, replacePartials action)
replacePartials (Lambda argData vars body, pos) = (Lambda argData vars $ replacePartials body, pos)
replacePartials (Funcall expr args, pos) = maybeLambda pos (\vals -> (Funcall (replacePartials expr) vals, pos)) args
replacePartials (TypeCheck (tested, typeDecl) expr, pos) = 
    (TypeCheck (replacePartials tested, typeDecl) $ replacePartials expr, pos)

maybeLambda :: SourcePos -> ([EveExpr] -> EveExpr) -> [EveExpr] -> EveExpr
maybeLambda pos body args =
  if numParams > 0
    then (Lambda argExpr (Just $ args2Vars argExpr) . body $ substParams lambdaList args, pos)
    else body $ map replacePartials args
  where
    numParams = length . filter ((== Variable "?") . fst) $ args
    lambdaList = map (("__" ++) . (: [])) $ take numParams ['a'..]
    substParams :: [String] -> [EveExpr] -> [EveExpr]
    substParams (next:params) ((Variable "?", _):args) = 
        (Variable next, pos) : substParams params args
    substParams params (arg:args) = replacePartials arg : substParams params args
    substParams params [] = []
    argExpr = ArgExpr lambdaList [] Nothing

-- argList comes in reversed, which is good for our purposes
parseArgList :: [(String, Maybe EveExpr, Maybe EveType)] -> EveExpr -> ([String], [(String, EveExpr)], EveExpr)
parseArgList argList body@(_, pos) = (reverse $ names, argDefaults, newBody)
  where 
    newBody = foldr makeTypeCheck body argList
    (names, defaults, types) = unzip3 argList
    argDefaults = map unpack $ filter hasDefault $ zip names defaults
    hasDefault (name, Nothing) = False
    hasDefault (name, Just argDefault) = True
    unpack (name, Just val) = (name, val)
    unpack pair = error $ "Bad default argument: " ++ show pair
    makeTypeCheck (varName, _, Nothing) body = body
    makeTypeCheck (varName, _, Just typeDecl) body = 
        (TypeCheck ((Variable varName, pos), typeDecl) body, pos)

makeLambda pos (rawArgs, varargs) body = (Lambda argExpr (Just $ args2Vars argExpr) newBody, pos)
  where
    (args, defaults, newBody) = parseArgList rawArgs body
    argExpr = ArgExpr args defaults varargs

findLastExpr (docString, defLines) = (lines, docString, last)
  where
    (lines, [(NakedExpr last, pos)]) = splitAt (length defLines - 1) defLines

funcall pos name args = (Funcall (Variable name, pos) args, pos)
methodcall pos name obj args = 
    (Funcall (funcall pos "attr" [obj, (Literal $ makeString name, pos)]) args, pos)

happyError ((token, pos):whatever) = withPos pos $ throwEveError $ ParseError token 
}
