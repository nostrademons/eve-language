{
module Parser(parseRepl, parseFile, EveData(..), EveExpr(..)) where
import Control.Monad.Error hiding (join)
import Lexer
import Data
}

%name file File
%name replLine ReplLine
%monad { EveM } 
%tokentype { (AlexPosn, EveToken) }

%token
INT   { (_, TokInt $$) }
BOOL  { (_, TokBool $$) }
STR   { (_, TokString $$) }
VAR   { (_, TokVar $$) }
EOL   { (_, TokNewline) }
'**'  { (_, TokOp "**") }
'*'   { (_, TokOp "*") }
'/'   { (_, TokOp "/") }
'%'   { (_, TokOp "%") }
'+'   { (_, TokOp "+") }
'-'   { (_, TokKeyword "-") }
'<'   { (_, TokOp "<") }
'>'   { (_, TokOp ">") }
'<='  { (_, TokOp "<=") }
'>='  { (_, TokOp ">=") }
'=='  { (_, TokOp "==") }
'!='  { (_, TokOp "!=") }
'='   { (_, TokOp "=")  }
'and' { (_, TokOp "and") }
'or'  { (_, TokOp "or") }
'not' { (_, TokOp "not") }
'if'  { (_, TokKeyword "if") }
'then'{ (_, TokOp "then") }
'else'{ (_, TokOp "else") }
'to'  { (_, TokVar "to") }
','   { (_, TokKeyword ",") }
'.'   { (_, TokOp ".") }
'|'   { (_, TokKeyword "|") }
':'   { (_, TokKeyword ":") }
'('   { (_, TokDelim '(') }
')'   { (_, TokDelim ')') }
'{'   { (_, TokDelim '{') }
'}'   { (_, TokDelim '}') }
'['   { (_, TokDelim '[') }
']'   { (_, TokDelim ']') }
'import' { (_, TokKeyword "import") }
'export' { (_, TokKeyword "export") }

%left 'and' 'or'
%nonassoc 'not'
%nonassoc '!=' '==' '>=' '<=' '>' '<'
%left '+' '-'
%left '*' '/' '%'
%left '**'
%left NEG

%%

File : FileLine { [$1] }
     | File EOL FileLine { $3 : $1 }

FileLine : VAR '=' Expr           { Binding $1 $3 }
         | 'import' DottedIdent { Import (reverse $2) }
         | 'export' VarList     { Export (reverse $2) "" }
         | 'export' VarList 'to' DottedIdent 
                                { Export (reverse $2) (join "." (reverse $4)) }

ReplLine : Expr                 { Expr $1 }
         | 'import' DottedIdent { ReplImport (reverse $2) }
         | VAR '=' Expr         { Assignment $1 $3 }

Expr : Operand             { $1 }
     | '-' Expr %prec NEG  { binop "-" (Literal (Int 0)) $2 }
     | Expr '**' Expr { binop "**" $1 $3 }
     | Expr '*' Expr { binop "*" $1 $3 }
     | Expr '/' Expr { binop "/" $1 $3 }
     | Expr '%' Expr { binop "%" $1 $3 }
     | Expr '+' Expr { binop "+" $1 $3 }
     | Expr '-' Expr { binop "-" $1 $3 }
     | Expr '==' Expr { binop "==" $1 $3 }
     | Expr '!=' Expr { binop "!=" $1 $3 }
     | Expr '>' Expr  { binop ">" $1 $3 }
     | Expr '<' Expr  { binop "<" $1 $3 }
     | Expr '>=' Expr { binop ">=" $1 $3 }
     | Expr '<=' Expr { binop "<=" $1 $3 }
     | Expr 'and' Expr { binop "and" $1 $3 }
     | Expr 'or' Expr { binop "or" $1 $3 }
     | 'not' Expr { Funcall (Variable "\\not") [$2] }
     | Expr '[' Expr ']' { Funcall (Variable "get") [$3, $1] }
     | 'if' Expr 'then' Expr 'else' Expr
       { Cond [($2, $4), (Literal (Bool True), $6)] }


Operand     : INT                          { (Literal . Int) $1 }
            | BOOL                         { (Literal . Bool) $1 }
            | STR                          { (Literal . String) $1 }
            | VAR                          { Variable $1 }
            | '[' ']'                      { ListLiteral [] }
            | '[' ExprList ']'             { ListLiteral (reverse $2) }
            | '(' Expr ')'                 { $2 }
            | Expr '(' ExprList ')'        { Funcall $1 (reverse $3) }
            | '{' '|' VarList '|' Expr '}' { Lambda (reverse $3) $5 }

DottedIdent : VAR { [$1] }
            | DottedIdent '.' VAR      { $3 : $1 }
ExprList    : Expr                     { [$1] }
            | ExprList ',' Expr        { $3 : $1 }
VarList     : VAR                      { [$1] }
            | VarList ',' VAR          { $3 : $1 }


{

parseFile input = file input >>= return . map replaceFilePartials 
parseRepl input = replLine input >>= return . replaceReplPartials

replaceFilePartials :: EveFileLine -> EveFileLine
replaceFilePartials (Binding var expr) = Binding var $ replacePartials expr
replaceFilePartials line = line

replaceReplPartials :: EveReplLine -> EveReplLine
replaceReplPartials (Expr expr) = Expr $ replacePartials expr
replaceReplPartials (Assignment var expr) = Assignment var $ replacePartials expr
replaceReplPartials line = line

-- Also TODO: Expand the legal positions so that eg. if statements can also have partials
replacePartials :: EveExpr -> EveExpr
replacePartials (Literal val) = Literal val
replacePartials (ListLiteral args) = maybeLambda ListLiteral args
replacePartials (Variable var) = Variable var
replacePartials (Cond condList) = Cond $ map handleClause condList
  where handleClause (pred, action) = (replacePartials pred, replacePartials action)
replacePartials (Lambda args body) = Lambda args $ replacePartials body
replacePartials (Funcall expr args) = maybeLambda (Funcall (replacePartials expr)) args

maybeLambda exprConstr args =
  if numParams > 0
    then Lambda lambdaList . exprConstr $ substParams lambdaList args
    else exprConstr (map replacePartials args)
  where
    numParams = length . filter (== Variable "?") $ args
    lambdaList = map (("__" ++) . (: [])) $ take numParams ['a'..]
    substParams :: [String] -> [EveExpr] -> [EveExpr]
    substParams (next:params) (Variable "?":args) = Variable next : substParams params args
    substParams params (arg:args) = replacePartials arg : substParams params args
    substParams params [] = []

binop name left right = Funcall (Variable ('\\':name)) [left, right]

happyError ((posn, token):whatever) = throwError $ ParseError token posn
}
