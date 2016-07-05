module SymbolicCalc.Evaluator (evaluate, SymbolTable) where

    import qualified Data.Map as MapLib
    import SymbolicCalc.Parser
    import SymbolicCalc.Lexer

    type SymbolTable = MapLib.Map String Double

    evaluate :: Node -> SymbolTable -> (Double, SymbolTable)
    evaluate (SumNode op left right) table =
        let (left', table')     = evaluate left table
            (right', table'')   = evaluate right table'
        in case op of
                Add -> (left' + right', table'')
                Sub -> (left' - right', table'')
    evaluate (ProdNode op left right) table =
        let (left', table')     = evaluate left table
            (right', table'')    = evaluate right table'
        in case op of
                Mul -> (left' * right', table'')
                Div -> (left' / right', table'')
    evaluate (UnaryNode op node) table =
        let (val, table') = evaluate node table
        in case op of
                Add -> (val, table')
                Sub -> (-val, table')
    evaluate (NumNode num) table = (num, table)
    evaluate (VarNode var) table = lookUp var table
    evaluate (AssignNode var node) table =
        let (v, table') = evaluate node table
            (_, table'') = addSymbol var v table'
        in (v, table'')

    lookUp :: String -> SymbolTable -> (Double, SymbolTable)
    lookUp varName table =
        case MapLib.lookup varName table of
            Just v -> (v, table)
            Nothing -> error $ "Undefined variable " ++ varName

    addSymbol :: String -> Double -> SymbolTable -> ((), SymbolTable)
    addSymbol varName varVal table =
        let table' = MapLib.insert varName varVal table
        in ((), table')
