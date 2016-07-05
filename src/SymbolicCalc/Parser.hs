module SymbolicCalc.Parser (Node(..), parse) where

    import SymbolicCalc.Lexer

    data Node   = SumNode Operator Node Node
                | ProdNode Operator Node Node
                | AssignNode String Node
                | UnaryNode Operator Node
                | NumNode Double
                | VarNode String
        deriving (Show)

    expression :: [Token] -> (Node, [Token])
    expression tokens =
        let (termTree, tokens') = term tokens
        in
            case lookAhead tokens' of
                (TokenOperation op) | elem op [Add, Sub] ->
                    let (exprTree, tokens'') = expression (accept tokens')
                    in (SumNode op termTree exprTree, tokens'')
                TokenAssignment ->
                    case termTree of
                        VarNode str ->
                            let (exTree, tokens'') = expression (accept tokens')
                            in (AssignNode str exTree, tokens'')
                        _ -> error "Only variables can be assigned to"
                _ -> (termTree, tokens')

    term :: [Token] -> (Node, [Token])
    term tokens =
        let (facTree, tokens') = factor tokens
        in
            case lookAhead tokens' of
                (TokenOperation op) | elem op [Mul, Div] ->
                    let (termTree, tokens'') = term (accept tokens')
                    in (ProdNode op facTree termTree, tokens'')
                _ -> (facTree, tokens')

    factor :: [Token] -> (Node, [Token])
    factor tokens =
        case lookAhead tokens of
            (TokenNumber num)       -> (NumNode num, accept tokens)
            (TokenIdentifier id)    -> (VarNode id, accept tokens)
            (TokenOperation op) | elem op [Add, Sub] ->
                let (facTree, tokens') = factor (accept tokens)
                in (UnaryNode op facTree, tokens')
            TokenLeftParen          ->
                let (expTree, tokens') = expression (accept tokens)
                in
                    if lookAhead tokens' /= TokenRightParen
                        then error "Missing right parenthesis"
                        else (expTree, accept tokens')
            _ -> error $ "Parse error on token: " ++ show tokens

    parse :: [Token] -> Node
    parse tokens =
        let (tree, tokens') = expression tokens
        in
            if null tokens'
                then tree
                else error $ "Leftover tokens: " ++ show tokens'
