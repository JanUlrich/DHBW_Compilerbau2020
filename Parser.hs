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

-- [String] -> ParserResult Expression [String]
parseExpression :: Parser Token Expression
parseExpression tokens = parseTExpression +.+ parseE'xpression
                            >>> (\(e1, e2) -> Addition e1 e2) $ tokens
parseE'xpression tokens = (isToken PlusOperator) +.+ parseTExpression +.+ parseE'xpression
                            >>> (\((_, e1), e2) -> Addition e1 e2)
                            ||| --- oder epsilon
                            (\a -> case a of
                                 [] -> State (Number 0) []
                                 _  -> Failure)  $ tokens
parseTExpression (IntLiteral a : ls) = State (Number a) ls

-- [String] -> ParserResult Expression [String]
evaluateExpression :: Parser Token Int
evaluateExpression tokens = evaluateTExpression +.+ evaluateE'xpression
                            >>> (\(e1, e2) -> e1 + e2) $ tokens
evaluateE'xpression tokens = (isToken PlusOperator) +.+ evaluateTExpression +.+ evaluateE'xpression
                            >>> (\((_, e1), e2) -> e1 + e2)
                            ||| --- oder epsilon
                            (\a -> case a of
                                 [] -> State 0 []
                                 _  -> Failure)  $ tokens
evaluateTExpression (IntLiteral a : ls) = State a ls
