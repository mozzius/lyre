{
module Language.Parser where
import Language.Syntax
import Language.Lexer
import Data.List
}

%name program Program
%tokentype { Token }
%error { parseError }

%token
  let           { TokenLet _ }
  if            { TokenIf _ }
  else          { TokenElse _ }
  var           { TokenVar _ $$ }
  int           { TokenInt _ $$ }
  stringLiteral { TokenStringLiteral _ $$ }
  bool          { TokenBoolean _ $$ }
  return        { TokenReturn _ }
  func          { TokenFunc _ }
  funcName      { TokenFuncCall _ $$ }
  ':'           { TokenColon _ }
  ','           { TokenComma _ }
  '!'           { TokenNot _ }
  '='           { TokenAssign _ }
  '+'           { TokenPlus _ }
  '-'           { TokenMinus _ }
  '*'           { TokenTimes _ }
  '/'           { TokenDiv _ }
  '('           { TokenLParen _ }
  ')'           { TokenRParen _ }
  '{'           { TokenLCurly _ }
  '}'           { TokenRCurly _ }
  '||'          { TokenDisj _ }
  '&&'          { TokenConj _ }
  '<'           { TokenLThan _ }
  '>'           { TokenGThan _ }
  '=='          { TokenEq _ }
  '!='          { TokenNEq _ }
  '<='          { TokenLEq _ }
  '>='          { TokenGEq _ } 
  nl            { TokenNL _ }

%%

Program :: { Stmts }
  : Function nl OptNL Program                   { $1:$4 }
  | Function nl OptNL                           { [$1] }
  | Function                                    { [$1] }

Block :: { Block }
  : Expression                                   { Expr $1 }
  | '{' OptNL Statements '}'                     { Curly $3 }

OptStatements :: { Stmts }
  : Statements                                   { $1 }
  |                                              { [] }

Statements :: { Stmts }
  : Statement nl OptNL Statements                { $1:$4 }
  | Statement nl OptNL                           { [$1] }
  | Statement                                    { [$1] }

Statement :: { Stmt }
  : Assignment                                   { $1 }
  | return Expression                            { Return $2 }
  | Function                                     { $1 }
  | If                                           { $1 }

Type :: { Type }
  : var Types                                    { Type ($1:$2) }

Types :: { [String] }
  : var Types                                    { $1:$2 }
  |                                              { [] }

OptType :: { Type }
  : ':' Type                                     { $2 }
  |                                              { Untyped }

Assignment :: { Stmt }
  : let var OptType '=' Expression               { Let $2 $3 $5 }

If :: { Stmt }
  : if '(' Expression ')' Block else If          { IfElseIf $3 $5 $7 }
  | if '(' Expression ')' Block else Block       { IfElse $3 $5 $7 }
  | if '(' Expression ')' Block                  { If $3 $5 }

Function :: { Stmt }
  : func funcName OptArguments ')' OptType Block { FuncDef (init $2) $3 $5 $6 }

OptArguments :: { [Argument] }
  : Arguments                                    { $1 }
  |                                              { [] }

Arguments :: { [Argument] }
  : var ':' Type ',' Arguments                   { (Arg $1 $3):$5 }
  | var ':' Type                                 { [Arg $1 $3] }

OptExprList :: { [Expr] }
  : ExprList                                     { $1 }
  |                                              { [] }

ExprList :: { [Expr] }
  : Expression ',' ExprList                      { $1:$3 }
  | Expression                                   { [$1] }

Expression :: { Expr }
  : Expression '||' Expression                   { BinOp Or $1 $3 }
  | Conjunction                                  { $1 }

Conjunction :: { Expr }
  : Conjunction '&&' Conjunction                 { BinOp And $1 $3 }
  | Equality                                     { $1 }

Equality :: { Expr }
  : Equality '==' Equality                       { BinOp Equals $1 $3 }
  | Equality '!=' Equality                       { BinOp NotEquals $1 $3 }
  | Comparison                                   { $1 }

Comparison :: { Expr }
  : Equality '<' Equality                        { BinOp LessThan $1 $3 }
  | Equality '<=' Equality                       { BinOp LessEq $1 $3 }
  | Equality '>' Equality                        { BinOp GreaterThan $1 $3 }
  | Equality '>=' Equality                       { BinOp GreaterEq $1 $3 }
  | Sum                                          { $1 }

Sum :: { Expr }
  : Sum '+' Term                                 { BinOp Plus $1 $3 }
  | Sum '-' Term                                 { BinOp Minus $1 $3 }
  | Term                                         { $1 }

Term :: { Expr }
  : Term '*' Factor                              { BinOp Times $1 $3 }
  | Term '/' Factor                              { BinOp Div $1 $3 }
  | Factor                                       { $1 }

Inversion :: { Expr }
  : '!' Inversion                                { UnaOp Inv $2 }
  | Factor                                       { $1 }

Factor :: { Expr }
  : var                                          { Var $1 }
  | int                                          { Int (read $1) }
  | bool                                         { Boolean ($1 == "true") }
  | stringLiteral                                { String (init . tail $ $1) }
  | '(' Expression ')'                           { Brack $2 }
  | funcName OptExprList ')'                     { FuncCall (init $1) $2 }

OptNL
  : OptNL nl                                     {}
  |                                              {}

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
