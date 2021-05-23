{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators   #-}

import qualified Data.Set as Set
import Data.Set (fromList, Set)
import Test.QuickCheck
import GDP

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

-- Now using GDP to guarantee that getNodeNames is never used in tree with Bepa

-- Must not export this
newtype NoBepa tree = NoBepa Defn
type role NoBepa nominal

-- Must export this
noBepaValidatorTypeB :: TypeB -> Either String (TypeB ~~ NoBepa validated)
noBepaValidatorTypeB tree = case (checkBepaTypeB tree) of
  True -> Left "Error: contains Bepa"
  False -> Right $ defn tree

checkBepaTypeB :: TypeB -> Bool
checkBepaTypeB (TypeB _ nodeName listTree) = case nodeName of
  (NodeName "Bepa") -> True
  nodeName -> foldl validation False listTree
  where
    validation b t = checkBepaTypeB t || b

noBepaValidatorTree :: Tree -> Either String (Tree ~~ NoBepa validated)
noBepaValidatorTree tree = case (checkBepaTree tree) of
  True -> Left "Error: contains Bepa"
  False -> Right $ defn tree

checkBepaTree :: Tree -> Bool
checkBepaTree (Tree_TypeA nodeInfo _ listTree)  = case (nodeInfoName nodeInfo) of
  (NodeName "Bepa") -> True
  nodeName -> foldl validation False listTree
  where
    validation b t = checkBepaTree t || b
checkBepaTree (Tree_TypeB typeB) = checkBepaTypeB typeB

-- Can't use not proved Bepa free data here
getNodeNamesTypeB :: (TypeB ~~ NoBepa validated) -> Set NodeName
getNodeNamesTypeB (The typeB) = getNodeNamesTypeBUnsafe typeB

-- Must not export this because it is unsafe
getNodeNamesTypeBUnsafe :: TypeB -> Set NodeName
getNodeNamesTypeBUnsafe (TypeB _ nodeName listTypeB) = Set.insert nodeName (getNodeNamesListBUnsafe listTypeB)

getNodeNamesListBUnsafe :: [TypeB] -> Set NodeName
getNodeNamesListBUnsafe (x:xs) = Set.union (getNodeNamesTypeBUnsafe x) $ getNodeNamesListBUnsafe xs
getNodeNamesListBUnsafe [] = Set.empty

-- Can't use not proved Bepa free data here
getNodeNamesTree :: (Tree ~~ NoBepa validated) -> Set NodeName
getNodeNamesTree (The tree) = getNodeNamesTreeUnsafe tree

-- Must not export this because it is unsafe
getNodeNamesTreeUnsafe :: Tree -> Set NodeName
getNodeNamesTreeUnsafe (Tree_TypeA nodeInfo _ listTree) = case (nodeInfoName nodeInfo) of
  nodeName -> Set.insert nodeName (getNodeNamesListTreeUnsafe listTree)
getNodeNamesTreeUnsafe (Tree_TypeB typeB) = getNodeNamesTypeBUnsafe typeB

getNodeNamesListTreeUnsafe :: [Tree] -> Set NodeName
getNodeNamesListTreeUnsafe (x:xs) = Set.union (getNodeNamesTreeUnsafe x) $ getNodeNamesListTreeUnsafe xs
getNodeNamesListTreeUnsafe [] = Set.empty

useNoBepaProvedGetNodeNamesTypeB typeB =
  case noBepaValidatorTypeB typeB of
    Left e -> Left e
    Right provedNoBepa  -> Right $ getNodeNamesTypeB provedNoBepa

useNoBepaProvedGetNodeNamesTree tree =
  case noBepaValidatorTree tree of
    Left e -> Left e
    Right provedNoBepa  -> Right $ getNodeNamesTree provedNoBepa

prop_proved_no_Bepa_getNodeNamesTypeB tree = withMaxSuccess 5000 $ result === False
  where
    result = case useNoBepaProvedGetNodeNamesTypeB tree of
      Left _ -> False
      Right nodeNames -> elem (NodeName "Bepa") nodeNames

prop_proved_no_Bepa_getNodeNamesTree tree = withMaxSuccess 5000 $ result === False
  where
    result = case useNoBepaProvedGetNodeNamesTree tree of
      Left _ -> False
      Right nodeNames -> elem (NodeName "bepa") nodeNames
