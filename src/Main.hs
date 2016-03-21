module Main where

import Functions
import Types
import Tree
import InfinitStructure

main = do
    print $ take' 10 numbers
    print $ treeTakeDepth 4 nullTree
    print $ treeTakeDepth 4 infTreeTwo
