{
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Lexer (Token(..),scanTokens,symString,constrString) where

import Data.Text (Text)
import GHC.Generics (Generic)

}

%wrapper "posn"

$digit  = 0-9
$alpha  = [a-zA-Z\_\-\=]
$lower  = [a-z]
$upper  = [A-Z]
$eol    = [\n]
$alphanum  = [$alpha $digit \_]
@sym    = $lower ($alphanum | \')*
@constr = ($upper ($alphanum | \')* | \(\))
@int    = \-? $digit+
@charLiteral = \' ([\\.]|[^\']| . ) \'
@stringLiteral = \"(\\.|[^\"]|\n)*\"

@langPrag = [a-z]+

tokens :-

  $white*$eol                   { \p s -> TokenNL p }
  $eol+                         { \p s -> TokenNL p }
  $white+                       ;
  "--".*                        ;
  @constr                       { \p s -> TokenConstr p s }
  lang\.@langPrag               { \p s -> TokenLang p s }
  forall                        { \p _ -> TokenForall p }
  let                           { \p s -> TokenLet p }
  in                            { \p s -> TokenIn p }
  succ                          { \p s -> TokenSucc p }
  zero                          { \p s -> TokenZero p }
  natcase                       { \p s -> TokenNatCase p }
  case                          { \p s -> TokenCase p }
  of                            { \p s -> TokenOf p }
  fix                           { \p s -> TokenFix p }
  fst                           { \p s -> TokenFst p }
  snd                           { \p s -> TokenSnd p }
  inl                           { \p s -> TokenInl p }
  inr                           { \p s -> TokenInr p }
  "|"                           { \p s -> TokenSep p }
  @sym				                  { \p s -> TokenVar p s }
  @int				                  { \p s -> TokenInt p s }
  "->"                          { \p s -> TokenArrow p }
  \\                            { \p s -> TokenDiv p }
  \/\\                          { \p s -> TokenTyLambda p }
  \=                            { \p s -> TokenEq p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  \:                            { \p s -> TokenSig p }
  "?"                           { \p _ -> TokenHole p }
  "*"                           { \p s -> TokenTimes p }
  "+"                           { \p s -> TokenPlus p }
  "-"                           { \p s -> TokenMinus p }
  "<"                           { \p s -> TokenLPair p }
  ">"                           { \p s -> TokenRPair p }
  ", "                          { \p s -> TokenMPair p }
  \.                            { \p _ -> TokenDot p }
  \@                            { \p _ -> TokenAt p }

{

data Token
  = TokenLang     AlexPosn String
  | TokenCase     AlexPosn
  | TokenNatCase  AlexPosn
  | TokenOf       AlexPosn
  | TokenSep      AlexPosn
  | TokenFix      AlexPosn
  | TokenLet      AlexPosn
  | TokenIn       AlexPosn
  | TokenTyLambda AlexPosn
  | TokenLambda   AlexPosn
  | TokenSym      AlexPosn String
  | TokenZero     AlexPosn
  | TokenSucc     AlexPosn
  | TokenArrow    AlexPosn
  | TokenEq       AlexPosn
  | TokenLParen   AlexPosn
  | TokenRParen   AlexPosn
  | TokenNL       AlexPosn
  | TokenConstr   AlexPosn String
  | TokenSig      AlexPosn
  | TokenEquiv    AlexPosn
  | TokenHole     AlexPosn
  | TokenTimes    AlexPosn
  | TokenPlus     AlexPosn
  | TokenLPair    AlexPosn
  | TokenRPair    AlexPosn
  | TokenMPair    AlexPosn
  | TokenFst      AlexPosn
  | TokenSnd      AlexPosn
  | TokenInl      AlexPosn
  | TokenInr      AlexPosn
  | TokenForall   AlexPosn
  | TokenDot      AlexPosn
  | TokenAt       AlexPosn
  | TokenVar      AlexPosn String
  | TokenDiv      AlexPosn
  | TokenInt      AlexPosn String
  | TokenMinus    AlexPosn
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x
symString _ = error "Not a symbol"

constrString :: Token -> String
constrString (TokenConstr _ x) = x

scanTokens = alexScanTokens >>= (return . trim)

trim :: [Token] -> [Token]
trim = reverse . trimNL . reverse . trimNL

trimNL :: [Token] -> [Token]
trimNL [] = []
trimNL (TokenNL _ : ts) = trimNL ts
trimNL ts = ts

}