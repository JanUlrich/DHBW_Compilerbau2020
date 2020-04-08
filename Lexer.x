{
module Lexer  (toTokens, Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

      $white+ ; 
      "//".*  ;

      \'$alpha\'	{ \s ->  CHARLITERAL ((\(_:snd:_) -> snd) s) }
      \+\+ 		{ \s -> INCREMENT }
	\+		{ \s -> PLUS }
	\-\-		{ \s -> DECREMENT }
	\-\>		{ \s -> ARROW }
	\-		{ \s -> MINUS }
	\*		{ \s -> MUL }
	\/		{ \s -> DIV }
	\%		{ \s -> MOD }
	\&\&		{ \s -> AND }
	\|\|		{ \s -> OR }
	\(		{ \s -> LBRACE }
	\)		{ \s -> RBRACE }
	\{		{ \s -> LBRACKET }
	\}		{ \s -> RBRACKET }
	\=\=		{ \s -> EQUAL }
	\!\=		{ \s -> NOTEQUAL }
	\<\=		{ \s -> LESSEQUAL }
	\>\=		{ \s -> GREATEREQUAL }
	\<		{ \s -> LESS }
	\>		{ \s -> GREATER }
	\;		{ \s -> SEMICOLON }
	\,		{ \s -> COLON }
	\.		{ \s -> DOT }
	\=		{ \s -> ASSIGN }
	\#		{ \s -> SHARP }

      abstract         { \s -> ABSTRACT }
      boolean          { \s -> BOOLEAN }
      break		{ \s  ->  BREAK }
      case		{ \s  ->  CASE }
      catch		{ \s  ->  CATCH }
      char 		{ \s  ->  CHAR  }
      class		{ \s  ->  CLASS}
      continue		{ \s  ->  CONTINUE }
      default		{ \s  ->  DEFAULT }
      do		{ \s  ->  DO }
      else		{ \s  ->  ELSE }
      extends		{ \s  ->  EXTENDS }
      finally		{ \s  ->  FINALLY }
      for		{ \s  ->  FOR }
      if		{ \s  ->  IF }
      instanceof	{ \s  ->  INSTANCEOF }
      int		{ \s  ->  INT }
      new		{ \s  ->  NEW }
      private		{ \s  ->  PRIVATE }
      protected		{ \s  ->  PROTECTED }
      public		{ \s  ->  PUBLIC }
      return		{ \s  ->  RETURN }
      static		{ \s  ->  STATIC }
      super		{ \s  ->  SUPER }
      switch		{ \s  ->  SWITCH }
      this		{ \s  ->  THIS }
      throw		{ \s  ->  THROW }
      throws		{ \s  ->  THROWS }
      try		{ \s  ->  TRY }
      void		{ \s  ->  VOID }
      while		{ \s  ->  WHILE }
      true		{ \s  -> BOOLLITERAL(True) }
      false		{ \s  -> BOOLLITERAL(False) }
      null		{ \s  ->  JNULL }
      $digit+       { \s -> INTLITERAL (read s) }
      $alpha [$alpha $digit \_ \']*   { \s -> IDENTIFIER s }
      
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
