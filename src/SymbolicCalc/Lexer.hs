module SymbolicCalc.Lexer (Operator(..), Token(..), tokenize, lookAhead, accept) where

    import Data.Char

    data Operator = Add | Sub | Mul | Div
        deriving (Show, Eq)

    operatorToStr :: Operator -> String
    operatorToStr Add = "+"
    operatorToStr Sub = "-"
    operatorToStr Mul = "*"
    operatorToStr Div = "/"

    operator :: Char -> Operator
    operator c
        | c == '+' = Add
        | c == '-' = Sub
        | c == '*' = Mul
        | c == '/' = Div

    data Token  = TokenOperation Operator
                | TokenIdentifier String
                | TokenNumber Double
                | TokenLeftParen
                | TokenRightParen
                | TokenAssignment
                | TokenEnd
        deriving (Show, Eq)

    tokenToStr :: Token -> String
    tokenToStr (TokenOperation op)  = operatorToStr op
    tokenToStr (TokenIdentifier id) = id
    tokenToStr (TokenNumber num)    = show num

    tokenize :: String -> [Token]
    tokenize [] = []
    tokenize (c:str)
        | elem c "+-*/" = TokenOperation (operator c) : tokenize str
        | c == '('      = TokenLeftParen : tokenize str
        | c == ')'      = TokenRightParen : tokenize str
        | c == '='      = TokenAssignment : tokenize str
        | isDigit c     = number c str
        | isAlpha c     = identifier c str
        | isSpace c     = tokenize str
        | otherwise     = error $ "Cannot tokenize " ++ [c]

    number :: Char -> String -> [Token]
    number c str = let (digs, rest) = span isDigit str in TokenNumber (read (c:digs)) : tokenize rest

    identifier :: Char -> String -> [Token]
    identifier c str = let (ident, rest) = span isAlphaNum str in TokenIdentifier (c:ident) : tokenize rest

    lookAhead :: [Token] -> Token
    lookAhead []                = TokenEnd
    lookAhead (token:tokens)    = token

    accept :: [Token] -> [Token]
    accept []               = error "Nothing to accept"
    accept (token:tokens)   = tokens
