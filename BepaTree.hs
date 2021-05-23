import qualified Data.Set as Set
import Data.Set (fromList, Set)
import Test.QuickCheck

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

-- End of part 1 of the task

-- Now some property based test for part 2
instance Arbitrary NodeInfo where
  arbitrary = do
    name <- arbitrary
    return (NodeInfo { cost = Cost 0, nodeInfoName = name })

instance Arbitrary NodeName where
  arbitrary = do
    name <- frequency [(4, arbitrary), (1, return "SomeString"), (1, return "Bepa"), (1, return "bepa")]
    return (NodeName name)

instance Arbitrary TypeB where
  arbitrary = frequency [(4, sized genTypeB), (1, genLeafTybeB)]
              where
                genTypeB size = do
                  nodeName <- arbitrary
                  n <- choose (0, size `div` 2)
                  listTypeB <- vectorOf n (genTypeB n)
                  return (TypeB (Cost 0) nodeName listTypeB)
                genLeafTybeB = do
                  nodeName <- arbitrary
                  return (TypeB (Cost 0) nodeName [])

instance Arbitrary Tree where
  arbitrary = sized genTree

genTree :: Int -> Gen Tree
genTree size = frequency [(4, genTypeA), (1, genLeafTypeA), (4, genTypeB)]
  where
    genLeafTypeA = do
      nodeName <- arbitrary
      return (Tree_TypeA nodeName "SomeString" [])
    genTypeA = do
      nodeName <- arbitrary
      n <- choose (0, size `div` 2)
      listTree <- vectorOf n (genTree n)
      return (Tree_TypeA nodeName "SomeString" listTree)
    genTypeB = do
      treeB <- arbitrary
      return (Tree_TypeB treeB)

prop_there_is_pepa = forAll (resize 10 arbitrary) $ \tree1 tree2 ->
  let result = getCommonNodeNamesExceptBepa tree1 tree2 in elem (NodeName "bepa") result === False

prop_no_Bepa_getNodeNamesBExceptBepa tree = withMaxSuccess 1000 $ ( elem (NodeName "Bepa") result ) === False
  where
    result = getNodeNamesTypeBExceptBepa tree

prop_no_Bepa_getNodeNamesExceptBepa tree = withMaxSuccess 1000 $ ( elem (NodeName "Bepa") result ) === False
  where
    result = getNodeNamesTreeExceptBepa tree

prop_is_intersection tree1 tree2 = Set.intersection result_1 result_2 === fromList result_3
  where
    result_1 = getNodeNamesTreeExceptBepa tree1
    result_2 = getNodeNamesTreeExceptBepa tree2
    result_3 = getCommonNodeNamesExceptBepa tree1 tree2

prop_elem_both tree1 tree2 = elem nodeName result_1 ==> elem nodeName result_2 ==> elem nodeName result_3
  where
    result_1 = getNodeNamesTreeExceptBepa tree1
    result_2 = getNodeNamesTypeBExceptBepa tree2
    result_3 = getNodeNamesTreeExceptBepa tree3
    nodeName = NodeName "SomeString"
    tree3 = Tree_TypeA (NodeInfo (Cost 0) (NodeName "")) "" [tree1, (Tree_TypeB tree2)]

prop_elem_only_one tree1 tree2 = elem nodeName result_1 ==> not (elem nodeName result_2) ==> not $ elem nodeName result_3
  where
    result_1 = getNodeNamesTreeExceptBepa tree1
    result_2 = getNodeNamesTreeExceptBepa tree2
    result_3 = getCommonNodeNamesExceptBepa tree1 tree2
    nodeName = NodeName "SomeString"
