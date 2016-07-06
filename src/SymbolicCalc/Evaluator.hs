module SymbolicCalc.Evaluator (evaluate, SymbolTable, Evaluator(..)) where

    import qualified Data.Map as MapLib
    import SymbolicCalc.Parser
    import SymbolicCalc.Lexer

    type SymbolTable = MapLib.Map String Double

    newtype Evaluator a = Ev (Either String a)

    instance Monad Evaluator where
        (Ev ev) >>= continuation =
            case ev of
                Left message    -> Ev (Left message)
                Right value     -> continuation value
        return val = Ev (Right val)
        fail message = Ev (Left message)

    evaluate :: Node -> SymbolTable -> Evaluator (Double, SymbolTable)
    evaluate (SumNode op left right) table = do
        (left', table')     <- evaluate left table
        (right', table'')   <- evaluate right table'
        case op of
            Add -> return (left' + right', table'')
            Sub -> return (left' - right', table'')

    evaluate (ProdNode op left right) table = do
        (left', table')     <- evaluate left table
        (right', table'')   <- evaluate right table'
        case op of
            Mul -> return (left' * right', table'')
            Div -> return (left' / right', table'')

    evaluate (UnaryNode op node) table = do
        (val, table') <- evaluate node table
        case op of
            Add -> return (val, table')
            Sub -> return (-val, table')

    evaluate (NumNode num) table = return (num, table)

    evaluate (VarNode var) table = lookUp var table

    evaluate (AssignNode var node) table = do
        (v, table')     <- evaluate node table
        (_, table'')    <- addSymbol var v table'
        return (v, table'')

    lookUp :: String -> SymbolTable -> Evaluator (Double, SymbolTable)
    lookUp varName table =
        case MapLib.lookup varName table of
            Just v -> return (v, table)
            Nothing -> fail ("Undefined variable " ++ varName)

    addSymbol :: String -> Double -> SymbolTable -> Evaluator ((), SymbolTable)
    addSymbol varName varVal table =
        let table' = MapLib.insert varName varVal table
        in return ((), table')
