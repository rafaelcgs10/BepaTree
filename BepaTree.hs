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
getCommonNodeNamesExceptBepa _ _ = undefined
