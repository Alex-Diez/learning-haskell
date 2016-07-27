module FunctionallySolvingProblems.ShortestPath (tests) where

    import Test.Hspec

    data Section = Section {
        getA :: Int,
        getB :: Int,
        getC :: Int
    } deriving (Show)

    type RoadSystem = [Section]

    data Label = A | B | C deriving (Show, Eq)

    type Path = [(Label, Int)]

    optimalPath :: RoadSystem -> Path
    optimalPath roads =
        let (bestAPath, bestBPath) = foldl roadStep ([], []) roads
        in if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath

    roadStep :: (Path, Path) -> Section -> (Path, Path)
    roadStep (pathA, pathB) (Section a b c) =
        let priceA = sum $ map snd pathA
            priceB = sum $ map snd pathB
            forwardPriceToA = priceA + a
            crossPriceToA = priceB + b + c
            forwardPriceToB = priceB + b
            crossPriceToB = priceA + a + c
            newPathToA = if forwardPriceToA <= crossPriceToA
                            then (A, a):pathA
                            else (C, c):(B, b):pathB
            newPathToB = if forwardPriceToB <= crossPriceToB
                            then (B, b):pathB
                            else (C, c):(A, a):pathA
        in (newPathToA, newPathToB)

    tests = do
        it "returns ([(C, 30), (B, 10)], [(B, 10)]) when given [Section 50 10 30]"
            (roadStep ([], []) (Section 50 10 30) == ([(C, 30), (B, 10)], [(B, 10)]))

        it "returns [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8), (C, 0)] when given [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]"
            (optimalPath [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0] == [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8), (C, 0)])
