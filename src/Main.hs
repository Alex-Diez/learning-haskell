module Main where

    import Test.Hspec
    import FunctionallySolvingProblems.ReversePolishCalc
    import FunctionallySolvingProblems.ShortestPath

    main = do
        hspec FunctionallySolvingProblems.ReversePolishCalc.tests
        hspec FunctionallySolvingProblems.ShortestPath.tests
