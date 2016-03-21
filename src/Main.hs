module Main where

import Functions
import Types
import Tree

main = do
    functions
    types

    print $ Tree.treeFromList [15,34,1,3,5,7,2,4,23,45,8]

    where
        functions = do
            print(evenSumInt [1..100])
            print(evenSumIntegral [1..100])
            print(evenSumMatch [1..100])
            print(evenSumEta [1..100])
            print(evenSumHof [1..100])
            print(evenSumHofFold [1..100])
            print(evenSumHofEagerFold [1..100])
            print(evenSumLambdas [1..100])
            print(evenSumLambdasOp [1..100])
            print(evenSumLambdasEta [1..100])

            print(squareEvenSum [1..100])
            print(squareEvenSum' [1..100])
            print(squareEvenSum'' [1..100])

        types = do
            print(showInfos name color)
            print(showInfosTypes nameType colorType)
            print(z)
            print(c)
