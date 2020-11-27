{
module Parser where
import Exprs
import Lexer
import Data.List
}

%name program StatementsEmptyable
%tokentype { Token }
%error { parseError }

%token 
  let     { TokenLet _ }
  var     { TokenVar _ $$ }
  int     { TokenInt _ $$ }
  return  { TokenReturn _ }
  func    { TokenFunc _ }
  '!'     { TokenNot _ }
  '='     { TokenEq _ }
  '+'     { TokenPlus _ }
  '-'     { TokenMinus _ }
  '*'     { TokenTimes _ }
  '/'     { TokenDiv _ }
  '('     { TokenLParen _ }
  ')'     { TokenRParen _ }
  '{'     { TokenLCurly _ }
  '}'     { TokenRCurly _ }
  '||'    { TokenDisj _ }
  '&&'    { TokenConj _ }
  nl      { TokenNL _ }

%%

Block
  : Expression                              { Expr $1 }
  | '{' OptNL StatementsEmptyable OptNL '}' { Curly $3 }

StatementsEmptyable
  : Statements                              { $1 }
  |                                         { [] }

Statements
  : Statement nl OptNL Statements           { $1:$4 }
  | Statement                               { [$1] }

Statement
  : Assignment                              { $1 }
  | return Expression                       { Return $2 }
  | func var '(' ')' Block                  { FuncDef $2 $5 }

Assignment
  : let var '=' Expression                  { Let $2 $4 }

Expression
  : Expression '||' Expression              { BinOp Or $1 $3 }
  | Conjunction                             { $1 }

Conjunction
  : Conjunction '&&' Conjunction            { BinOp And $1 $3 }
  | Inversion                               { $1 }

Inversion
  : '!' Inversion                           { UnaOp Inv $2 }
  | Sum                                     { $1 }

Sum
  : Sum '+' Term                            { BinOp Plus $1 $3 }
  | Sum '-' Term                            { BinOp Minus $1 $3 }
  | Term                                    { $1 }

Term
  : Term '*' Factor                         { BinOp Times $1 $3 }
  | Term '/' Factor                         { BinOp Div $1 $3 }
  | Factor                                  { $1 }

Factor
  : var                                     { Var $1 }
  | int                                     { Int (read $1) }
  | '(' Expression ')'                      { Brack $2 }

OptNL
  : nl OptNL                                {}
  |                                         {}


{

parseError :: [Token] -> a
parseError tokens = error 
  $ let (token, line, col) = getPos $ head tokens
    in "Parse error: unexpected " ++ token ++ " at line " ++ show line ++ ", column " ++ show col ++ "\n" ++ concat (map (\t -> show t) tokens)
parseError [] = error "No tokens found"

parseProgram :: String -> Stmts
parseProgram input = program $ scanTokens input

}