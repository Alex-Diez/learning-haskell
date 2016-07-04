module Basic.Types where

-- type synonims
type Name   = String
type Color  = String

showInfos :: Name -> Color -> String
showInfos name color = "Name: " ++ name ++ ", Color: " ++ color

-- const Name
name :: Name
name = "Robin"
-- const Color
color :: Color
color = "Blue"

-- programmer's type
data NameType = NameConstr String
data ColorType = ColorConstr String

showInfosTypes :: NameType -> ColorType -> String
showInfosTypes (NameConstr name) (ColorConstr color) =
    "Name: " ++ name ++ ", Color: " ++ color

nameType = NameConstr "Robin"
colorType = ColorConstr "Blue"

-- constructors
data Complex = Complex { real :: Int
                       , img :: Int
                       } deriving (Show)
c = Complex 1 2
z = Complex { real = 3, img = 4 }

--recurrent types
data List l = Empty | Cons l (List l)

-- infix notation
infixr 5 :::
data InfixList l =
    Nil | l ::: (InfixList l)
        deriving(Show, Read,Eq,Ord)

-- binary tree
data BinTree t = EmptyTree
                 | NodeTree t (BinTree t) (BinTree t)
                 deriving(Eq,Ord)

treeFromList :: (Ord o) => [o] -> BinTree o
treeFromList [] = EmptyTree
treeFromList (x:xs) =
    NodeTree x
    (treeFromList (filter (<x) xs))
    (treeFromList (filter (>x) xs))
