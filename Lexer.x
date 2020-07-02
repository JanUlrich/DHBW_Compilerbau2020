{module Lexer (toTokens, Token(..)) where}
%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  [=]					{ \s -> Equals }
  [\{]					{ \s -> BraceOpen }
  [\}]					{ \s -> BraceClose }
  [\"][^\n\"]+[\"]		{ \s -> Data s }
  [^\n=\}\{ \t]+		{ \s -> Data s }

{
data Token =
  BraceOpen    |
	BraceClose  |
  Equals |
  Data String
	deriving (Eq,Show)

toTokens s = alexScanTokens s
}
