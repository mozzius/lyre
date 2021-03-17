{
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Lexer (Token(..),scanTokens,symString,getPos) where

import Data.Text (Text)
import GHC.Generics (Generic)

}

%wrapper "posn"

$digit         = 0-9
$alpha         = [a-zA-Z\_]
$lower         = [a-z]
$upper         = [A-Z]
$eol           = [\n]
$alphanum      = [$alpha $digit]
@sym           = $alpha($alphanum)*
@application   = $alpha($alphanum)*\(
@int           = \-? $digit+
@charLiteral   = \' ([\\.]|[^\']| . ) \'
@stringLiteral = \"(\\.|[^\"]|\n)*\"
@boolean       = (true|false)

@langPrag = [a-z]+

tokens :-

  $white*$eol                   { \p s -> TokenNL p }
  $eol+                         { \p s -> TokenNL p }
  " "+                          ;
  "//".*                        ;
  lang\.@langPrag               { \p s -> TokenLang p s }
  let                           { \p _ -> TokenLet p }
  func                          { \p _ -> TokenFunc p }
  return                        { \p _ -> TokenReturn p }
  if                            { \p s -> TokenIf p }
  else                          { \p s -> TokenElse p }
  int                           { \p _ -> TokenIntType p }
  string                        { \p _ -> TokenStringType p }
  bool                          { \p _ -> TokenBoolType p }
  channel                       { \p _ -> TokenChannelType p }
  @boolean                      { \p s -> TokenBoolean p s }
  @sym				                  { \p s -> TokenVar p s }
  @int				                  { \p s -> TokenInt p s }
  @stringLiteral                { \p s -> TokenStringLiteral p s }
  \\                            { \p _ -> TokenDiv p }
  @application                  { \p s -> TokenApp p s }
  \(                            { \p _ -> TokenLParen p }
  \)                            { \p _ -> TokenRParen p }
  \{                            { \p _ -> TokenLCurly p }
  \}                            { \p _ -> TokenRCurly p }
  \=                            { \p _ -> TokenAssign p }
  \:                            { \p _ -> TokenColon p }
  "*"                           { \p _ -> TokenTimes p }
  "+"                           { \p _ -> TokenPlus p }
  "-"                           { \p _ -> TokenMinus p }
  "<"                           { \p _ -> TokenLThan p }
  ">"                           { \p _ -> TokenGThan p }
  "=="                          { \p _ -> TokenEq p }
  "<="                          { \p _ -> TokenLEq p }
  ">="                          { \p _ -> TokenGEq p }
  "!="                          { \p _ -> TokenNEq p }
  "||"                          { \p _ -> TokenDisj p }
  "&&"                          { \p _ -> TokenConj p }
  "!"                           { \p _ -> TokenNot p }
  ","                           { \p _ -> TokenComma p }
  "->"                          { \p _ -> TokenArrow p }

{

data Token
  = TokenLang          AlexPosn String
  | TokenLet           AlexPosn
  | TokenSym           AlexPosn String
  | TokenLParen        AlexPosn
  | TokenRParen        AlexPosn
  | TokenNL            AlexPosn
  | TokenColon         AlexPosn
  | TokenTimes         AlexPosn
  | TokenPlus          AlexPosn
  | TokenLThan         AlexPosn
  | TokenGThan         AlexPosn
  | TokenAssign        AlexPosn
  | TokenEq            AlexPosn
  | TokenGEq           AlexPosn
  | TokenLEq           AlexPosn
  | TokenNEq           AlexPosn
  | TokenDiv           AlexPosn
  | TokenVar           AlexPosn String
  | TokenInt           AlexPosn String
  | TokenApp           AlexPosn String
  | TokenMinus         AlexPosn
  | TokenLCurly        AlexPosn
  | TokenRCurly        AlexPosn
  | TokenDisj          AlexPosn
  | TokenConj          AlexPosn
  | TokenFunc          AlexPosn
  | TokenNot           AlexPosn
  | TokenReturn        AlexPosn
  | TokenComma         AlexPosn
  | TokenStringLiteral AlexPosn String
  | TokenIf            AlexPosn
  | TokenElse          AlexPosn
  | TokenBoolean       AlexPosn String
  | TokenIntType       AlexPosn
  | TokenBoolType      AlexPosn
  | TokenStringType    AlexPosn
  | TokenChannelType   AlexPosn
  | TokenArrow         AlexPosn
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x
symString _ = error "Not a symbol"

scanTokens = alexScanTokens >>= (return . trim)

getPos :: Token -> (String, Int, Int)
getPos (TokenDiv           (AlexPn _ line col)  ) = ("Div",         line, col)
getPos (TokenLang          (AlexPn _ line col) _) = ("Lang",        line, col)
getPos (TokenLet           (AlexPn _ line col)  ) = ("Let",         line, col)
getPos (TokenSym           (AlexPn _ line col) _) = ("Sym",         line, col)
getPos (TokenEq            (AlexPn _ line col)  ) = ("Eq",          line, col)
getPos (TokenLParen        (AlexPn _ line col)  ) = ("LParen",      line, col)
getPos (TokenRParen        (AlexPn _ line col)  ) = ("RParen",      line, col)
getPos (TokenNL            (AlexPn _ line col)  ) = ("NL",          line, col)
getPos (TokenColon         (AlexPn _ line col)  ) = ("Colon",       line, col)
getPos (TokenTimes         (AlexPn _ line col)  ) = ("Times",       line, col)
getPos (TokenPlus          (AlexPn _ line col)  ) = ("Plus",        line, col)
getPos (TokenLThan         (AlexPn _ line col)  ) = ("LThan",       line, col)
getPos (TokenGThan         (AlexPn _ line col)  ) = ("GThan",       line, col)
getPos (TokenAssign        (AlexPn _ line col)  ) = ("Assign",      line, col)
getPos (TokenEq            (AlexPn _ line col)  ) = ("Equals",      line, col)
getPos (TokenLEq           (AlexPn _ line col)  ) = ("LThanEq",     line, col)
getPos (TokenGEq           (AlexPn _ line col)  ) = ("GThanEq",     line, col)
getPos (TokenNEq           (AlexPn _ line col)  ) = ("NotEq",       line, col)
getPos (TokenVar           (AlexPn _ line col) _) = ("Var",         line, col)
getPos (TokenDiv           (AlexPn _ line col)  ) = ("Div",         line, col)
getPos (TokenInt           (AlexPn _ line col) _) = ("Int",         line, col)
getPos (TokenMinus         (AlexPn _ line col)  ) = ("Minus",       line, col)
getPos (TokenLCurly        (AlexPn _ line col)  ) = ("LCurly",      line, col)
getPos (TokenRCurly        (AlexPn _ line col)  ) = ("RCurly",      line, col)
getPos (TokenDisj          (AlexPn _ line col)  ) = ("Disj",        line, col)
getPos (TokenConj          (AlexPn _ line col)  ) = ("Conj",        line, col)
getPos (TokenFunc          (AlexPn _ line col)  ) = ("Func",        line, col)
getPos (TokenNot           (AlexPn _ line col)  ) = ("Not",         line, col)
getPos (TokenReturn        (AlexPn _ line col)  ) = ("Return",      line, col)
getPos (TokenComma         (AlexPn _ line col)  ) = (",",           line, col)
getPos (TokenApp           (AlexPn _ line col) _) = ("Application", line, col)
getPos (TokenStringLiteral (AlexPn _ line col) _) = ("StringLit",   line, col)
getPos (TokenIf            (AlexPn _ line col)  ) = ("If",          line, col)
getPos (TokenElse          (AlexPn _ line col)  ) = ("Else",        line, col)
getPos (TokenBoolean       (AlexPn _ line col) _) = ("Boolean",     line, col)
getPos (TokenIntType       (AlexPn _ line col)  ) = ("IntType",     line, col)
getPos (TokenStringType    (AlexPn _ line col)  ) = ("StringType",  line, col)
getPos (TokenBoolType      (AlexPn _ line col)  ) = ("BoolType",    line, col)
getPos (TokenChannelType   (AlexPn _ line col)  ) = ("ChannelType", line, col)
getPos (TokenArrow         (AlexPn _ line col)  ) = ("Arrow",       line, col)


trim :: [Token] -> [Token]
trim = reverse . trimNL . reverse . trimNL

trimNL :: [Token] -> [Token]
trimNL [] = []
trimNL (TokenNL _ : ts) = trimNL ts
trimNL ts = ts

}