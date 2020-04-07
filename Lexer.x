{module Lexer (toTokens, Token(..)) where}
%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  $alpha+		{ \s -> IDENTIFIER (read s) }

{

data Token
     = ABSTRACT 
     | BOOLEAN 
     | BREAK 
     | CASE 
     | CATCH 
     | CHAR  
     | CLASS
     | CONTINUE 
     | DEFAULT 
     | DO 
     | ELSE 
     | EXTENDS 
     | FINALLY 
     | FOR 
     | IF 
     | INSTANCEOF 
     | INT
     | NEW 
     | PRIVATE 
     | PROTECTED 
     | PUBLIC 
     | RETURN 
     | STATIC 
     | SUPER 
     | SWITCH 
     | THIS 
     | THROW 
     | THROWS 
     | TRY 
     | VOID 
     | WHILE 
     | INTLITERAL Integer
     | BOOLLITERAL Bool
     | JNULL 
     | CHARLITERAL Char
     | STRINGLITERAL String
     | IDENTIFIER String
     | EQUAL 
     | LESSEQUAL 
     | GREATEREQUAL 
     | NOTEQUAL 
     | LOGICALOR 
     | LOGICALAND 
     | INCREMENT 
     | DECREMENT 
     | SHIFTLEFT 
     | SHIFTRIGHT 
     | UNSIGNEDSHIFTRIGHT 
     | SIGNEDSHIFTRIGHT 
     | PLUSEQUAL 
     | MINUSEQUAL 
     | TIMESEQUAL 
     | DIVIDEEQUAL 
     | ANDEQUAL 
     | OREQUAL 
     | XOREQUAL 
     | MODULOEQUAL 
     | SHIFTLEFTEQUAL 
     | SIGNEDSHIFTRIGHTEQUAL 
     | UNSIGNEDSHIFTRIGHTEQUAL 
     | LBRACE 
     | RBRACE 
     | LBRACKET 
     | RBRACKET 
     | LSQBRACKET 
     | RSQBRACKET 
     | SEMICOLON 
     | DOT 
     | ASSIGN 
     | LESS 
     | GREATER 
     | EXCLMARK 
     | TILDE 
     | QUESMARK 
     | COLON 
     | PLUS 
     | MINUS 
     | MUL 
     | DIV 
     | MOD 
     | AND 
     | OR 
     | XOR 
     | SHARP
     | ARROW
     deriving(Eq,Show)

toTokens s = alexScanTokens s
}
