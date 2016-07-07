module Monads.WhyNot where

    data WhyNot a = Nah | Sure a
        deriving Show

    instance Monad WhyNot where
        Sure a  >>= k = k a
        Nah     >>= _ = Nah
        return a    = Sure a
        fail _      = Nah

    safeRoot :: Double -> WhyNot Double
    safeRoot x =
        if x >= 0
            then return (sqrt x)
            else fail "Boo!"

    test :: Double -> WhyNot Double
    test x = do
        y <- safeRoot x
        z <- safeRoot (y-4)
        w <- safeRoot z
        return w

    main = do
        print $ test 9
        print $ test 400
