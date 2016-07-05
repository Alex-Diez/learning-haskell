module Main where

    import qualified Data.Map as Map
    import SymbolicCalc.Lexer
    import SymbolicCalc.Parser
    import SymbolicCalc.Evaluator

    main :: IO ()
    main = do
        let tokens = tokenize "x1 = -15 / (2 + x2)"
        print tokens
        let node = parse tokens
        print node
        let val = evaluate node Map.empty
        print val
