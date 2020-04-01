import Lexer

type Parser tok a = [tok] -> ParserResult tok a



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

(>>>) :: ([tok] -> ParserResult tok a) -> (a -> b) -> ([tok] -> ParserResult tok b)
(p >>> f) toks = case p toks of
    Failure -> Failure
    State a b -> State (f a) b


--Aufgabe 1 a)
evalOneExpression (IntLiteral a : PlusOperator : IntLiteral b : _) = a + b
evalOneExpression (IntLiteral a : MinusOperator : IntLiteral b : _) = a - b

data ParserResult tok a = Failure | State a [tok] 
                    deriving Show

--Aufgabe 1 b)
evalOneExpressionParser (IntLiteral a : PlusOperator : IntLiteral b : tokens) = State (a+b) tokens
evalOneExpressionParser (IntLiteral a : MinusOperator : IntLiteral b : tokens) = State (a-b) tokens
evalOneExpressionParser _ = Failure

--Aufgabe 2 a)
evalTwoExpressions = (evalOneExpressionParser +.+ evalOneExpressionParser)

--Aufgabe 2 b)
evalTwoExpressionsAddedResults = (evalOneExpressionParser +.+ evalOneExpressionParser) >>> pairToInt
pairToInt (a,b) = a + b 


data Expression = Addition Expression Expression
                | Subtraction Expression Expression
                | Number Int
                    deriving Show


parseExpression :: [Token] -> ParserResult Token Expression
parseExpression = error "To implement"

evaluateExpression :: [Token] -> ParserResult Token Int
evaluateExpression = evaluateTExpr +.+ evaluateExprPrime
                        >>> \(t, e) -> t + e

evaluateExprPrime :: [Token] -> ParserResult Token Int
evaluateExprPrime = (plusOperatorParser +.+ evaluateTExpr +.+ evaluateExprPrime
                        >>> \((_, t), e) -> t + e)
                    |||
                    (minusOperatorParser +.+ evaluateTExpr +.+ evaluateExprPrime
                        >>> \((_, t), e) -> (-t) + e)
                    ||| epsilonParser

evaluateTExpr :: [Token] -> ParserResult Token Int
evaluateTExpr (IntLiteral a : tokens) = State a tokens
evaluateTExpr _ = Failure

plusOperatorParser (PlusOperator : tokens) = State 0 tokens
plusOperatorParser _ = Failure

minusOperatorParser (MinusOperator : tokens) = State 0 tokens
minusOperatorParser _ = Failure

epsilonParser [] = State 0 []
epsilonParser _ = Failure





--Aufgabe 2 c)
many :: Parser tok a -> Parser tok [a]
many p tokens = case p tokens of
    Failure -> State [] tokens
    State p1Res state1 ->
        case many p state1 of
            Failure -> State [p1Res] state1
            State p2Res state2 -> State (p1Res: p2Res) state2

evalAddExpression (PlusOperator : IntLiteral b : tokens) = State b tokens
evalAddExpression _ = Failure
evalSubExpression (MinusOperator : IntLiteral b : tokens) = State (-b) tokens
evalSubExpression _ = Failure

-- Wertet beliebig lange Expression aus:
-- Beispiel: evalManyExpressions (toTokens "1 + 4 -2") ==> State 3 []
evalManyExpressions :: Parser Token Int
evalManyExpressions = evalOneExpressionParser +.+ many (evalAddExpression ||| evalSubExpression)
                        >>> \(a, b) -> a + (sum b)


