module Main where

    import Test.Hspec
    import FunctionallySolvingProblems.ReversePolishCalc

    main = do
        hspec FunctionallySolvingProblems.ReversePolishCalc.tests
