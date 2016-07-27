module FunctionallySolvingProblems.ReversePolishCalc (tests) where

    import Test.Hspec

    solve :: String -> Float
    solve expression = head (foldl folding [] (words expression))
        where
            folding :: [Float] -> String -> [Float]
            folding (arg1:arg2:args) "+"    = (arg1 + arg2) : args
            folding (arg1:arg2:args) "-"    = (arg2 - arg1) : args
            folding (arg1:arg2:args) "*"    = (arg1 * arg2) : args
            folding (arg1:arg2:args) "/"    = (arg2 / arg1) : args
            folding args number             = read number : args

    tests = do
        it "calculates single number"
            (solve "10" == 10)

        it "calculates addition"
            (solve "10 5 +" == 15)

        it "calculates subtraction"
            (solve "10 5 -" == 5)

        it "calculates multiplication"
            (solve "10 3 *" == 30)

        it "calculates division"
            (solve "10 2 /" == 5.0)

        it "calculates multiple operations"
            (solve "90 34 12 33 55 66 + * - + -" == 4037)
