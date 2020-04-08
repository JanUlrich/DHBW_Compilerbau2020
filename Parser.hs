import Lexer

type Parser tok a = [tok] -> ParserResult tok a
data ParserResult tok a = Failure | State a [tok] deriving Show

data Expression = Addition Expression Expression
                | Subtraction Expression Expression
                | Number Int
                    deriving Show

failure :: Parser a b
failure _ = Failure

succeed :: a -> Parser tok a
succeed value toks = State value toks

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy cond [] = Failure
satisfy cond (tok : toks) | cond tok = succeed tok toks
                          | otherwise = failure toks

isToken token = satisfy $ (==) token

-- nacheinander Erkennen
(+.+) :: Parser tok a -> Parser tok b -> Parser tok (a,b)
(p1 +.+ p2) toks = case p1 toks of
                    Failure -> Failure
                    State p1Res state1 ->
                        case p2 state1 of
                            Failure -> Failure
                            State p2Res state2 -> State (p1Res, p2Res) state2

-- Alternative
(|||) :: Parser tok a -> Parser tok a -> Parser tok a
(p1 ||| p2) toks = runAlternative $ p1 toks
    where
        runAlternative Failure = p2 toks
        runAlternative (State x y) = State x y

(>>>) :: Parser tok a -> (a -> b) -> Parser tok b
(p >>> f) toks = case p toks of
    Failure -> Failure
    State a b -> State (f a) b

many :: Parser tok a -> Parser tok [a]
many p tokens = case p tokens of
    Failure -> State [] tokens
    State p1Res state1 ->
        case many p state1 of
            Failure -> State [p1Res] state1
            State p2Res state2 -> State (p1Res: p2Res) state2

data Stmt = If(BExpr, Stmt , Maybe Stmt)
    | While( BExpr , Stmt )
    | Block([Stmt])
    | Empty
    | Return( Maybe Expr )        deriving (Show)

data BExpr = T
    deriving (Show)

data Expr = One
    deriving (Show)

isInt m (INTLITERAL n) = n == m
isInt _ _ = False

isBool v (BOOLLITERAL w) = v == w
isBool _ _ = False



stmt :: Parser Token Stmt
stmt = ifelsestmt
    ||| ifstmt 
    ||| whilestmt
    ||| block
    ||| emptystmt
    ||| returnstmt

ifstmt :: Parser Token Stmt
ifstmt = (isToken IF) +.+ (isToken LBRACE) >>> fst +.+ bexpression >>> snd
    +.+ (isToken RBRACE) >>> fst +.+ stmt
    >>> (\(bexpr, s) -> If(bexpr, s, Nothing))

ifelsestmt :: Parser Token Stmt
ifelsestmt = ((isToken IF) +.+ (isToken LBRACE) >>> fst +.+ bexpression >>> snd
 +.+ (isToken RBRACE) >>> fst +.+ stmt +.+ (isToken ELSE) >>> fst +.+ stmt)
 >>> (\((bexpr, s1), s2)-> If(bexpr, s1, Just s2))

whilestmt :: Parser Token Stmt
whilestmt = ((isToken WHILE) +.+ (isToken LBRACE) >>> fst +.+ bexpression >>> snd
 +.+ ((isToken RBRACE) +.+ stmt >>> snd)) 
 >>> (\(bexpr, s) -> While(bexpr, s))

block :: Parser Token Stmt
block = (((isToken LBRACKET) +.+ (isToken RBRACKET)) 
    >>> (\(_, _) -> Block([])))
    ||| (((isToken LBRACKET) +.+ stmts +.+ (isToken RBRACKET))
        >>> (\((_, s), _) -> Block(s)))

stmts :: Parser Token [Stmt]
stmts = many stmt

emptystmt :: Parser Token Stmt
emptystmt = (isToken SEMICOLON) >>> (\_ -> Empty)

returnstmt :: Parser Token Stmt
returnstmt = (((isToken RETURN) +.+ (isToken SEMICOLON)) 
    >>> (\(_, _) -> Return Nothing))
    ||| (((isToken RETURN) +.+ expression +.+ (isToken SEMICOLON)) 
    >>> (\((_, e), _) -> Return (Just e)))


bexpression = (satisfy (isBool True)) >>> (\_ -> T)
expression = (satisfy (isInt 1)) >>> (\_ -> One)

parser :: String -> Maybe Stmt
parser input = case stmt $ toTokens input of
    State stmt [] -> Just stmt
    _ -> Nothing

main = putStrLn $ show $ parser "while ( true ) { while ( true ) { if (true) { return; } else {return 1; } } }" 