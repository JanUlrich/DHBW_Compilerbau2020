{module Lexer (toTokens, Token(..)) where}
%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  [\+]					{ \s -> PlusOperator }
  [\-]					{ \s -> MinusOperator }
  $digit+		{ \s -> IntLiteral (read s) }

{
data Token =
  PlusOperator     |
  MinusOperator    |
	IntLiteral Int
	deriving (Eq,Show)

toTokens s = alexScanTokens s
}
