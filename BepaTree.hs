import qualified Data.Set as Set
import Data.Set (fromList, Set)

data NodeInfo = NodeInfo { cost :: Cost
                         , nodeInfoName :: NodeName
                         } deriving (Show, Eq, Ord)
newtype NodeName = NodeName String deriving (Show, Eq, Ord)

newtype Cost = Cost Float deriving (Show, Eq, Ord)

data Tree = Tree_TypeA NodeInfo String [Tree] | Tree_TypeB TypeB deriving (Show, Eq, Ord)

data TypeB = TypeB Cost NodeName [TypeB] deriving (Show, Eq, Ord)

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa tree1 tree2 = Set.toList (Set.intersection (getNodeNamesTreeExceptBepa tree1) (getNodeNamesTreeExceptBepa tree2))

getNodeNamesTreeExceptBepa :: Tree -> Set NodeName
getNodeNamesTreeExceptBepa (Tree_TypeA nodeInfo _ listTree) = case (nodeInfoName nodeInfo) of
  (NodeName "Bepa") -> getNodeNamesListTreeExceptBepa listTree
  nodeName -> Set.insert nodeName (getNodeNamesListTreeExceptBepa listTree)
getNodeNamesTreeExceptBepa (Tree_TypeB typeB) = getNodeNamesTypeBExceptBepa typeB

getNodeNamesListTreeExceptBepa :: [Tree] -> Set NodeName
getNodeNamesListTreeExceptBepa (x:xs) = Set.union (getNodeNamesTreeExceptBepa x) $ getNodeNamesListTreeExceptBepa xs
getNodeNamesListTreeExceptBepa [] = Set.empty

getNodeNamesTypeBExceptBepa :: TypeB -> Set NodeName
getNodeNamesTypeBExceptBepa (TypeB _ (NodeName "Bepa") listTypeB) = getNodeNamesListTypeBExceptBepa listTypeB
getNodeNamesTypeBExceptBepa (TypeB _ nodeName listTypeB) = Set.insert nodeName (getNodeNamesListTypeBExceptBepa listTypeB)

getNodeNamesListTypeBExceptBepa :: [TypeB] -> Set NodeName
getNodeNamesListTypeBExceptBepa (x:xs) = Set.union (getNodeNamesTypeBExceptBepa x) $ getNodeNamesListTypeBExceptBepa xs
getNodeNamesListTypeBExceptBepa [] = Set.empty
