{
{-# OPTIONS_GHC -w #-}
module LineLexicalScanner (Token(..),scanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$allChars = $printable # [\# \\ \/ \*]

tokens :-

  "/*"                                     { \s -> TokenCommentBegin }
  "*/"                                     { \s -> TokenCommentEnd }
  "#"                                      { \s -> TokenPlasmaCutter }
  $alpha [$alpha $digit $white $allChars]* { \s -> TokenText s }

{

data Token = TokenCommentBegin
           | TokenCommentEnd
           | TokenPlasmaCutter
           | TokenText String
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
