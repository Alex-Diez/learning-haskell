module Main where

    import qualified Data.Map as Map
    import SymbolicCalc.Lexer
    import SymbolicCalc.Parser
    import SymbolicCalc.Evaluator

    main :: IO ()
    main = do
        loop (Map.fromList [("pi", pi), ("e", exp 1.0)])

    loop table = do
        str <- getLine
        if null str
            then return ()
            else
                let tokens = tokenize str
                    node = parse tokens
                    Evaluator action = evaluate node
                    (val, table') = action table
                in do
                    print val
                    loop table'
