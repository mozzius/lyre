{
module Language.Parser where
import Language.Syntax
import Language.Lexer
import Data.List
import Debug.Trace
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
  funcName      { TokenApp _ $$ }
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
  '->'          { TokenArrow _ }
  '~'           { TokenConcat _ }
  '::'          { TokenAssert _ }
  nl            { TokenNL _ }
  boolType      { TokenBoolType _ }
  intType       { TokenIntType _ }
  stringType    { TokenStringType _ }
  channelType   { TokenChannelType _ }

%%

Program :: { Stmts }
  : Function nl OptNL Program                   { $1:$4 }
  | Function nl OptNL                           { [$1] }
  | Function                                    { [$1] }

Block :: { Block }
  : Expression                                   { Inline $1 }
  | '{' OptNL OptStatements '}'                  { Curly $3 }

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
  | Expression                                   { Expr $1 }

Type :: { Type }
  : intType                                      { IntType }
  | stringType                                   { StringType }
  | boolType                                     { BoolType }
  | channelType Type                             { ChannelType $2 }
  | func TypeList OptReturnType                  { FuncType $2 $3 }

TypeList :: { [Type] }
  : Type ',' TypeList                            { $1:$3 }
  | Type                                         { [$1] }
  |                                              { [] }

OptReturnType :: { OptType }
  : '->' Type                                    { Type $2 }
  |                                              { NoType }

OptType :: { OptType }
  : ':' Type                                     { Type $2 }
  |                                              { NoType }

Assignment :: { Stmt }
  : let var OptType '=' Expression               { if available $2 then Let $2 $3 $5 else error ("Parse error: \"" ++ $2 ++ "\" is a reserved name") }

If :: { Stmt }
  : if '(' Expression ')' Block else If          { IfElseIf $3 $5 $7 }
  | if '(' Expression ')' Block else Block       { IfElse $3 $5 $7 }
  | if '(' Expression ')' Block                  { If $3 $5 }

Function :: { Stmt }
  : func funcName OptArguments ')' OptType Block { if available (init $2) then FuncDef (init $2) $3 $5 $6 else error ("Parse error: \"" ++ (init $2) ++ "\" is a reserved name") }

OptArguments :: { [Argument] }
  : Arguments                                    { $1 }
  |                                              { [] }

Arguments :: { [Argument] }
  : var ':' Type ',' Arguments                   { (Arg $1 $3):$5 }
  | var ':' Type                                 { if available $1 then [Arg $1 $3] else error ("Parse error: \"" ++ $1 ++ "\" is a reserved name") }

OptExprList :: { [Expr] }
  : ExprList                                     { $1 }
  |                                              { [] }

ExprList :: { [Expr] }
  : Expression ',' ExprList                      { $1:$3 }
  | Expression                                   { [$1] }

Expression :: { Expr }
  : Expression '||' Conjunction                   { BinOp Or $1 $3 }
  | Conjunction                                  { $1 }

Conjunction :: { Expr }
  : Conjunction '&&' Equality                 { BinOp And $1 $3 }
  | Equality                                     { $1 }

Equality :: { Expr }
  : Equality '==' Comparison                       { BinOp Equals $1 $3 }
  | Equality '!=' Comparison                       { BinOp NotEquals $1 $3 }
  | Comparison                                   { $1 }

Comparison :: { Expr }
  : Comparison '<' Sum                        { BinOp LessThan $1 $3 }
  | Comparison '<=' Sum                       { BinOp LessEq $1 $3 }
  | Comparison '>' Sum                        { BinOp GreaterThan $1 $3 }
  | Comparison '>=' Sum                       { BinOp GreaterEq $1 $3 }
  | Sum                                          { $1 }

Sum :: { Expr }
  : Sum '+' Term                                 { BinOp Plus $1 $3 }
  | Sum '-' Term                                 { BinOp Minus $1 $3 }
  | Sum '~' Term                                 { BinOp Concat $1 $3 }
  | Term                                         { $1 }

Term :: { Expr }
  : Term '*' Inversion                              { BinOp Times $1 $3 }
  | Term '/' Inversion                              { BinOp Div $1 $3 }
  | Inversion                                    { $1 }

Inversion :: { Expr }
  : '!' Inversion                                { UnaOp Inv $2 }
  | Factor                                       { $1 }

Factor :: { Expr }
  : var                                          { if available $1 then Var $1 else error ("Parse error: \"" ++ $1 ++ "\" is a reserved name, and cannot be passed as a variable") }
  | int                                          { Int (read $1) }
  | bool                                         { Boolean ($1 == "true") }
  | stringLiteral                                { String (init . tail $ $1) }
  | '(' Expression ')'                           { Brack $2 }
  | funcName OptExprList ')'                     { App (init $1) $2 }
  | Expression '::' Type                         { Enforce $1 $3}

OptNL
  : OptNL nl                                     {}
  |                                              {}

{

available :: String -> Bool
available str = not $ str `elem` ["int", "str", "print", "length", "make", "spawn", "send", "recv"]

parseError :: [Token] -> a
parseError tokens = error
  $ let (token, line, col) = getPos $ head tokens
    in "Parse error: unexpected " ++ token
      ++ " at line " ++ show line
      ++ ", column " ++ show col
      ++ "\n" ++ concat (map (\t -> show t ++ "\n") tokens)
parseError [] = error "No tokens found"

parse :: String -> Stmts
parse input = program $ scanTokens input

}
