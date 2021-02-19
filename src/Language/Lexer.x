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
@funccall      = $alpha($alphanum)*\(
@int           = \-? $digit+
@charLiteral   = \' ([\\.]|[^\']| . ) \'
@stringLiteral = \"(\\.|[^\"]|\n)*\"

@langPrag = [a-z]+

tokens :-

  $white*$eol                   { \p s -> TokenNL p }
  $eol+                         { \p s -> TokenNL p }
  " "+                          ;
  "//".*                        ;
  lang\.@langPrag               { \p s -> TokenLang p s }
  let                           { \p _ -> TokenLet p }
  of                            { \p _ -> TokenOf p }
  func                          { \p _ -> TokenFunc p }
  return                        { \p _ -> TokenReturn p }
  @sym				                  { \p s -> TokenVar p s }
  @int				                  { \p s -> TokenInt p s }
  @stringLiteral                { \p s -> TokenStringLiteral p s }
  "->"                          { \p _ -> TokenArrow p }
  \\                            { \p _ -> TokenDiv p }
  \=                            { \p _ -> TokenEq p }
  @funccall                     { \p s -> TokenFuncCall p s }
  \(                            { \p _ -> TokenLParen p }
  \)                            { \p _ -> TokenRParen p }
  \{                            { \p _ -> TokenLCurly p }
  \}                            { \p _ -> TokenRCurly p }
  \:                            { \p _ -> TokenSig p }
  "*"                           { \p _ -> TokenTimes p }
  "+"                           { \p _ -> TokenPlus p }
  "-"                           { \p _ -> TokenMinus p }
  "<"                           { \p _ -> TokenLPair p }
  ">"                           { \p _ -> TokenRPair p }
  \.                            { \p _ -> TokenDot p }
  \@                            { \p _ -> TokenAt p }
  "||"                          { \p _ -> TokenDisj p }
  "&&"                          { \p _ -> TokenConj p }
  "!"                           { \p _ -> TokenNot p }
  ","                           { \p _ -> TokenComma p }

{

data Token
  = TokenLang          AlexPosn String
  | TokenCase          AlexPosn
  | TokenOf            AlexPosn
  | TokenSep           AlexPosn
  | TokenFix           AlexPosn
  | TokenLet           AlexPosn
  | TokenSym           AlexPosn String
  | TokenZero          AlexPosn
  | TokenArrow         AlexPosn
  | TokenEq            AlexPosn
  | TokenLParen        AlexPosn
  | TokenRParen        AlexPosn
  | TokenNL            AlexPosn
  | TokenSig           AlexPosn
  | TokenEquiv         AlexPosn
  | TokenHole          AlexPosn
  | TokenTimes         AlexPosn
  | TokenPlus          AlexPosn
  | TokenLPair         AlexPosn
  | TokenRPair         AlexPosn
  | TokenFst           AlexPosn
  | TokenSnd           AlexPosn
  | TokenInl           AlexPosn
  | TokenInr           AlexPosn
  | TokenForall        AlexPosn
  | TokenDot           AlexPosn
  | TokenAt            AlexPosn
  | TokenDiv           AlexPosn
  | TokenVar           AlexPosn String
  | TokenInt           AlexPosn String
  | TokenFuncCall      AlexPosn String
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
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x
symString _ = error "Not a symbol"

scanTokens = alexScanTokens >>= (return . trim)

getPos :: Token -> (String, Int, Int)
getPos (TokenDiv      (AlexPn _ line col)  ) = ("Div",       line, col)
getPos (TokenLang     (AlexPn _ line col) _) = ("Lang",      line, col)
getPos (TokenCase     (AlexPn _ line col)  ) = ("Case",      line, col)
getPos (TokenOf       (AlexPn _ line col)  ) = ("Of",        line, col)
getPos (TokenSep      (AlexPn _ line col)  ) = ("Sep",       line, col)
getPos (TokenFix      (AlexPn _ line col)  ) = ("Fix",       line, col)
getPos (TokenLet      (AlexPn _ line col)  ) = ("Let",       line, col)
getPos (TokenSym      (AlexPn _ line col) _) = ("Sym",       line, col)
getPos (TokenZero     (AlexPn _ line col)  ) = ("Zero",      line, col)
getPos (TokenArrow    (AlexPn _ line col)  ) = ("Arrow",     line, col)
getPos (TokenEq       (AlexPn _ line col)  ) = ("Eq",        line, col)
getPos (TokenLParen   (AlexPn _ line col)  ) = ("LParen",    line, col)
getPos (TokenRParen   (AlexPn _ line col)  ) = ("RParen",    line, col)
getPos (TokenNL       (AlexPn _ line col)  ) = ("NL",        line, col)
getPos (TokenSig      (AlexPn _ line col)  ) = ("Sig",       line, col)
getPos (TokenEquiv    (AlexPn _ line col)  ) = ("Equiv",     line, col)
getPos (TokenHole     (AlexPn _ line col)  ) = ("Hole",      line, col)
getPos (TokenTimes    (AlexPn _ line col)  ) = ("Times",     line, col)
getPos (TokenPlus     (AlexPn _ line col)  ) = ("Plus",      line, col)
getPos (TokenLPair    (AlexPn _ line col)  ) = ("LPair",     line, col)
getPos (TokenRPair    (AlexPn _ line col)  ) = ("RPair",     line, col)
getPos (TokenFst      (AlexPn _ line col)  ) = ("Fst",       line, col)
getPos (TokenSnd      (AlexPn _ line col)  ) = ("Snd",       line, col)
getPos (TokenInl      (AlexPn _ line col)  ) = ("Inl",       line, col)
getPos (TokenInr      (AlexPn _ line col)  ) = ("Inr",       line, col)
getPos (TokenForall   (AlexPn _ line col)  ) = ("Forall",    line, col)
getPos (TokenDot      (AlexPn _ line col)  ) = ("Dot",       line, col)
getPos (TokenAt       (AlexPn _ line col)  ) = ("At",        line, col)
getPos (TokenVar      (AlexPn _ line col) _) = ("Var",       line, col)
getPos (TokenDiv      (AlexPn _ line col)  ) = ("Div",       line, col)
getPos (TokenInt      (AlexPn _ line col) _) = ("Int",       line, col)
getPos (TokenMinus    (AlexPn _ line col)  ) = ("Minus",     line, col)
getPos (TokenLCurly   (AlexPn _ line col)  ) = ("LCurly",    line, col)
getPos (TokenRCurly   (AlexPn _ line col)  ) = ("RCurly",    line, col)
getPos (TokenDisj     (AlexPn _ line col)  ) = ("Disj",      line, col)
getPos (TokenConj     (AlexPn _ line col)  ) = ("Conj",      line, col)
getPos (TokenFunc     (AlexPn _ line col)  ) = ("Func",      line, col)
getPos (TokenNot      (AlexPn _ line col)  ) = ("Not",       line, col)
getPos (TokenReturn   (AlexPn _ line col)  ) = ("Return",    line, col)
getPos (TokenComma    (AlexPn _ line col)  ) = (",",         line, col)
getPos (TokenFuncCall (AlexPn _ line col) _) = ("FuncCall",  line, col)
getPos (TokenFuncCall (AlexPn _ line col) _) = ("StringLit", line, col)

trim :: [Token] -> [Token]
trim = reverse . trimNL . reverse . trimNL

trimNL :: [Token] -> [Token]
trimNL [] = []
trimNL (TokenNL _ : ts) = trimNL ts
trimNL ts = ts

}