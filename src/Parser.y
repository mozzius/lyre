{
module Parser where
import Exprs
import Lexer
}

%name program
%tokentype { Token }
%error { parseError }

%token 
  let  { TokenLet _ }
  in   { TokenIn _ }
  var  { TokenVar _ $$ }
  int  { TokenInt _ $$ }
  '='  { TokenEq _ }
  '+'  { TokenPlus _ }
  '-'  { TokenMinus _ }
  '*'  { TokenTimes _ }
  '/'  { TokenDiv _ }
  '('  { TokenLParen _ }
  ')'  { TokenRParen _ }

%%

Exp
  : let var '=' Exp in Exp  { Let $2 $4 $6 }
  | Exp1                    { Exp1 $1 }

Exp1
  : Exp1 '+' Term           { Plus $1 $3 }
  | Exp1 '-' Term           { Minus $1 $3 }
  | Term                    { Term $1 }

Term
  : Term '*' Factor         { Times $1 $3 }
  | Term '/' Factor         { Div $1 $3 }
  | Factor                  { Factor $1 }

Factor
  : int                     { Int (read $1) }
  | var                     { Var $1 }
  | '(' Exp ')'             { Brack $2 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseProgram :: String -> Exp
parseProgram input = program $ scanTokens input

}