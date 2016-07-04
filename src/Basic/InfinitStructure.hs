module Basic.InfinitStructure where

import Data.List

numbers :: [Integer]
numbers = 0:map (1+) numbers

take' n [] = []
take' 0 list = []
take' n (x:xs) = x:take' (n-1) xs

data BinTree bt = Empty
                  | Node bt (BinTree bt) (BinTree bt)
                        deriving(Eq,Ord)

treeFromList :: (Ord o) => [o] -> BinTree o
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))

instance (Show bt) => Show (BinTree bt)
    where
        show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
            where
                treeshow pref Empty = ""
                treeshow pref (Node x Empty Empty) = (pshow pref x)
                treeshow pref (Node x left Empty) = (pshow pref x) ++ "\n" ++
                                                    (showSon pref "`--" "   " left)
                treeshow pref (Node x Empty right) =
                              (pshow pref x) ++ "\n" ++
                              (showSon pref "`--" "   " right)
                treeshow pref (Node x left right) =
                              (pshow pref x) ++ "\n" ++
                              (showSon pref "|--" "|  " left) ++ "\n" ++
                              (showSon pref "`--" "   " right)
                showSon pref before next t = pref ++ before ++ treeshow (pref ++ next) t
                pshow pref x = replace '\n' ("\n" ++ pref) (show x)
                replace c new string = concatMap (change c new) string
                    where
                        change c new x
                            | x == c = new
                            | otherwise = x:[]

nullTree = Node 0 nullTree nullTree

treeTakeDepth _ Empty = Empty
treeTakeDepth 0 _     = Empty
treeTakeDepth n (Node x left right) = let
          nl = treeTakeDepth (n-1) left
          nr = treeTakeDepth (n-1) right
          in
              Node x nl nr

iTree = Node 0 (dec iTree) (inc iTree)
    where
            dec (Node x l r) = Node (x-1) (dec l) (dec r)
            inc (Node x l r) = Node (x+1) (inc l) (inc r)

treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f Empty = Empty
treeMap f (Node x left right) = Node (f x)
                                     (treeMap f left)
                                     (treeMap f right)

infTreeTwo :: BinTree Int
infTreeTwo = Node 0 (treeMap (\x -> x - 1) infTreeTwo)
                    (treeMap (\x -> x + 1) infTreeTwo)
