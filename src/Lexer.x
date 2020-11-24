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
  " "+                       ;
  "//".*                        ;
  @constr                       { \p s -> TokenConstr p s }
  lang\.@langPrag               { \p s -> TokenLang p s }
  let                           { \p _ -> TokenLet p }
  in                            { \p _ -> TokenIn p }
  of                            { \p _ -> TokenOf p }
  func                          { \p _ -> TokenFunc p }
  return                        { \p _ -> TokenReturn p }
  @sym				                  { \p s -> TokenVar p s }
  @int				                  { \p s -> TokenInt p s }
  "->"                          { \p _ -> TokenArrow p }
  \\                            { \p _ -> TokenDiv p }
  \=                            { \p _ -> TokenEq p }
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
  ", "                          { \p _ -> TokenMPair p }
  \.                            { \p _ -> TokenDot p }
  \@                            { \p _ -> TokenAt p }
  "||"                          { \p _ -> TokenDisj p }
  "&&"                          { \p _ -> TokenConj p }
  "!"                           { \p _ -> TokenNot p }

{

data Token
  = TokenLang     AlexPosn String
  | TokenCase     AlexPosn
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
  | TokenLCurly   AlexPosn
  | TokenRCurly   AlexPosn
  | TokenDisj     AlexPosn
  | TokenConj     AlexPosn
  | TokenFunc     AlexPosn
  | TokenNot      AlexPosn
  | TokenReturn   AlexPosn
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