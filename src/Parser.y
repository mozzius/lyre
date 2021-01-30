{
module Parser where
import Exprs
import Lexer
import Data.List
}

%name program OptStatements
%tokentype { Token }
%error { parseError }

%token
  let      { TokenLet _ }
  var      { TokenVar _ $$ }
  int      { TokenInt _ $$ }
  return   { TokenReturn _ }
  func     { TokenFunc _ }
  funccall { TokenFuncCall _ $$ }
  ','      { TokenComma _ }
  '!'      { TokenNot _ }
  '='      { TokenEq _ }
  '+'      { TokenPlus _ }
  '-'      { TokenMinus _ }
  '*'      { TokenTimes _ }
  '/'      { TokenDiv _ }
  '('      { TokenLParen _ }
  ')'      { TokenRParen _ }
  '{'      { TokenLCurly _ }
  '}'      { TokenRCurly _ }
  '||'     { TokenDisj _ }
  '&&'     { TokenConj _ }
  nl       { TokenNL _ }

%%

Block :: { Block }
  : Expression                              { Expr $1 }
  | '{' OptNL Statements '}'                { Curly $3 }

OptStatements :: { Stmts }
  : Statements                              { $1 }
  |                                         { [] }

Statements :: { Stmts }
  : Statement nl OptNL Statements           { $1:$4 }
  | Statement nl OptNL                      { [$1] }
  | Statement                               { [$1] }

Statement :: { Stmt }
  : Assignment                              { $1 }
  | return Expression                       { Return $2 }
  | func funccall OptArguments ')' Block    { FuncDef $2 $3 $5 }

Assignment :: { Stmt }
  : let var '=' Expression                  { Let $2 $4 }

OptArguments :: { [String] }
  : Arguments                               { $1 }
  |                                         { [] }

Arguments :: { [String] }
  : var ',' Arguments                       { $1:$3 }
  | var                                     { [$1] }

OptExprList :: { [Expr] }
  : ExprList                                { $1 }
  |                                         { [] }

ExprList :: { [Expr] }
  : Expression ',' ExprList                 { $1:$3 }
  | Expression                              { [$1] }

Expression :: { Expr }
  : Expression '||' Expression              { BinOp Or $1 $3 }
  | Conjunction                             { $1 }

Conjunction :: { Expr }
  : Conjunction '&&' Conjunction            { BinOp And $1 $3 }
  | Inversion                               { $1 }

Inversion :: { Expr }
  : '!' Inversion                           { UnaOp Inv $2 }
  | Sum                                     { $1 }

Sum :: { Expr }
  : Sum '+' Term                            { BinOp Plus $1 $3 }
  | Sum '-' Term                            { BinOp Minus $1 $3 }
  | Term                                    { $1 }

Term :: { Expr }
  : Term '*' Factor                         { BinOp Times $1 $3 }
  | Term '/' Factor                         { BinOp Div $1 $3 }
  | Factor                                  { $1 }

Factor :: { Expr }
  : var                                     { Var $1 }
  | int                                     { Int (read $1) }
  | '(' Expression ')'                      { Brack $2 }
  | funccall OptExprList ')'                { FuncCall $1 $2 }

OptNL
  : OptNL nl                                {}
  |                                         {}

{

parseError :: [Token] -> a
parseError tokens = error
  $ let (token, line, col) = getPos $ head tokens
    in "Parse error: unexpected " ++ token
      ++ " at line " ++ show line
      ++ ", column " ++ show col
      ++ "\n" ++ concat (map (\t -> show t ++ "\n") tokens)
parseError [] = error "No tokens found"

parseProgram :: String -> Stmts
parseProgram input = program $ scanTokens input

}
