import Lexer
import System.IO  
import Control.Monad

type Parser tok a = [tok] -> ParserResult tok a
data ParserResult tok a = Failure | State a [tok] deriving Show

data DataBlock = Block [Assertion]
                | Value String
                
data Assertion = Assert String DataBlock

instance Show Assertion where
    show (Assert s v) = "\"" ++ s ++ "\"" ++ ":" ++ (show v)

instance Show DataBlock where
  show (Block []) = "{ }"
  show (Block ass) = "{" ++ (foldl (\b a -> b ++ ",\n" ++ (show a)) (show $ head ass) (tail ass)) ++ "}"
  show (Value v) = v

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

findAssigns name ((Assert s (Value v)) : ls) = if name == s then 
        (v : (findAssigns name ls))
    else
        findAssigns name ls
findAssigns name ((Assert _ (Block v)) : ls) = (findAssigns name v) ++ (findAssigns name ls)
findAssigns _ [] = []

main = do  
        let list = []
        handle <- openFile "/tmp/data" ReadMode
        contents <- hGetContents handle
        let singlewords = contents
            State s ls = parse $ toTokens singlewords
            techs = findAssigns "\"tech_status\"" s
        putStr $ show (Block s)
        --print $ fst (splitAt 1000 ls)
        --print techs
        hClose handle   

parse :: Parser Token [Assertion]
parse = many (parseAssertion ||| parseAssertionBlock ) >>> concat

parseAssertion :: Parser Token [Assertion]
parseAssertion (Data d : Equals : ls) = case parseBlock ls of
    State block ls2 -> State [(Assert d (block))] ls2 
    _ -> Failure
parseAssertion _ = Failure

parseAssertionBlock = isToken BraceOpen +.+ parse +.+ isToken BraceClose >>> \((_, ass),_) -> ass

parseData (Data d : ls) = State (Value d) ls
parseData _ = Failure
parseBlock (Data d : ls) = State (Value d) ls
parseBlock (BraceOpen : BraceClose : ls) = State (Block []) ls
parseBlock (BraceOpen : Data d : BraceClose : ls) = State (Value d) ls
parseBlock (BraceOpen : ls) = ((many parseData +.+ isToken BraceClose >>> \(datas, _) -> (Value ("\"" ++ (show datas) ++ "\"")))
                                ||| (parse +.+ isToken BraceClose >>> \(assertions, _) -> Block assertions)) ls
parseBlock _ = Failure

{-
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
-}