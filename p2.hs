import Cp
import List  hiding (fac)
import Nat
import BTree
import LTree
import Probability
import ListUtils
import Show
import Data.List hiding (find)
import Test.QuickCheck hiding ((><),choose,collect)
import qualified Test.QuickCheck as QuickCheck
import System.Random  hiding (split)
import System.Process
import GHC.IO.Exception
import Graphics.Gloss
import Control.Monad
import Control.Applicative hiding ((<|>))
import Exp

emp x = Node(x,(Empty,Empty))

t7 = emp 7
t16 = emp 16
t7_10_16 = Node(10,(t7,t16))
t1_2_nil = Node(2,(emp 1, Empty)) 
t' = Node(5,(t1_2_nil, t7_10_16))

t0_2_1 = Node(2, (emp 0, emp 3))
t5_6_8 = Node(6, (emp 5, emp 8))
t2 = Node(4, (t0_2_1, t5_6_8))


dotBt :: (Show a) => BTree a -> IO ExitCode
dotBt = dotpict . bmap Just Just . cBTree2Exp . (fmap show)



maisDir = cataBTree g
  where g = either (const Nothing) dir_aux


dir_aux (a,(e,Nothing)) = Just a
dir_aux (a,(_,d)) = d 


maisEsq = cataBTree g
  where g = either (const Nothing) esq_aux

esq_aux (a,(Nothing, d)) = Just a
esq_aux (a,(e,_)) = e 



insOrd' x = cataBTree g 
  where g = undefined

insOrd a x = undefined




isOrd' = cataBTree g
  where g = undefined

isOrd l = (isOrd_aux.inordt) l

isOrd_aux :: (Eq a,Ord a) => [a] -> Bool
isOrd_aux (t) | (t == qSort t) = True
              | otherwise = False



rrot = undefined

lrot = undefined

splay l t =  undefined