module SymbolicCalc.Evaluator (evaluate, SymbolTable, Evaluator(..)) where

    import qualified Data.Map as MapLib
    import SymbolicCalc.Parser
    import SymbolicCalc.Lexer

    type SymbolTable = MapLib.Map String Double

    newtype Evaluator a = Evaluator (SymbolTable -> (a, SymbolTable))

    instance Monad Evaluator where
        (Evaluator action) >>= continuation = Evaluator $
            \table ->
                let (value, table') = action table
                    (Evaluator action') = continuation value
                in action' table'
        return val = Evaluator (\table -> (val, table))

    evaluate :: Node -> Evaluator Double
    evaluate (SumNode op left right) = do
        left'   <- evaluate left
        right'  <- evaluate right
        case op of
            Add -> return $ left' + right'
            Sub -> return $ left' - right'

    evaluate (ProdNode op left right) = do
        left'   <- evaluate left
        right'  <- evaluate right
        case op of
            Mul -> return $ left' * right'
            Div -> return $ left' / right'

    evaluate (UnaryNode op node) = do
        val <- evaluate node
        case op of
            Add -> return val
            Sub -> return (-val)

    evaluate (NumNode num) = return num

    evaluate (VarNode var) = lookUp var

    evaluate (AssignNode var node) = do
        v <- evaluate node
        addSymbol var v

    lookUp :: String -> Evaluator Double
    lookUp varName = Evaluator $ \table ->
        case MapLib.lookup varName table of
            Just v -> (v, table)
            Nothing -> error $ "Undefined variable " ++ varName

    addSymbol :: String -> Double -> Evaluator Double
    addSymbol varName varVal = Evaluator $ \table ->
        let table' = MapLib.insert varName varVal table
        in (varVal, table')
