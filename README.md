# Expression Parser
Quellcode zur Compilerbau Vorlesung vom 23.03.2020

## Lexer
* generieren mit `alex Lexer.x`

## Parser
* `ghci Parser.hs`
* Ausführung in der GHCI command-line.
    * `parseExpression $ toTokens "3 + 25 + 14"`
    * `evaluateExpression $ toTokens "3 + 25 + 14"`