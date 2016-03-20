module Functions where

import Data.List (foldl')

-- just for integer type
evenSumInt :: [Integer] -> Integer
evenSumInt list = accumSumInt 0 list
accumSumInt n list =
    if list == []
        then n
        else
            let x = head list
                xs = tail list
            in if even x
                then accumSumInt (n+x) xs
                else accumSumInt n xs

-- for all types
evenSumIntegral :: Integral t => [t] -> t
evenSumIntegral list = accumSum 0 list
    where
        accumSum n list =
            if list == []
                then n
                else
                    let x = head list
                        xs = tail list
                    in if even x
                        then accumSum (n+x) xs
                        else accumSum n xs

-- with pattern matching
evenSumMatch :: Integral t => [t] -> t
evenSumMatch list = accumSum 0 list
    where
        accumSum n [] = n
        accumSum n (x:xs) =
            if even x
                then accumSum (n+x) xs
                else accumSum n xs

-- eta notation
evenSumEta :: Integral t => [t] -> t
evenSumEta = accumSum 0
    where
        accumSum n [] = n
        accumSum n (x:xs) =
            if even x
                then accumSum (n+x) xs
                else accumSum n xs

-- High order function
evenSumHof :: Integral t => [t] -> t
evenSumHof list = sum 0 (filter even list)
    where
        sum n [] = n
        sum n (x:xs) = sum (n+x) xs

-- Hof with foldl
evenSumHofFold :: Integral t => [t] -> t
evenSumHofFold list = foldl sum 0 (filter even list)
    where
        sum acc value = acc + value

-- Hof with eager foldl'
evenSumHofEagerFold :: Integral t => [t] -> t
evenSumHofEagerFold list = foldl' sum 0 (filter even list)
    where
        sum acc value = acc + value

-- lambdas
evenSumLambdas :: Integral t => [t] -> t
evenSumLambdas list = foldl' (\acc val -> acc + val) 0 (filter even list)

-- lambdas with (+)
evenSumLambdasOp :: Integral t => [t] -> t
evenSumLambdasOp list = foldl' (+) 0 (filter even list)

-- lambdas with eta reduction
evenSumLambdasEta :: Integral t => [t] -> t
evenSumLambdasEta = (foldl' (+) 0) . (filter even)

-- use abstraction
sum' :: (Num n) => [n] -> n
sum' = foldl' (+) 0
evenSum :: Integral t => [t] -> t
evenSum = sum' . (filter even)

-- even sum usage
squareEvenSum = sum' . (filter even) . (map (^2))
squareEvenSum' = evenSum . (map (^2))
squareEvenSum'' = sum' . (map (^2)) . (filter even)
