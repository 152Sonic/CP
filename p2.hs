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



--isOrd

--isOrd' = cataBTree g
--  where g = either (const (True,Empty))  isOrd_a


--Node(a,(l,r)) -> (a,((Bool1,btree1),(Bool2,btree2))) -> (bolla,Node(a,(e,d))
--isOrd_a (a,((b1,Empty),(b2,Empty))) = if(b1 && b2) then (True, Node(a,(Empty,Empty)))  else (False, Node(a,(Empty,Empty)))
--isOrd_a (a,( (b1, Node (e,(l1,r1))),(b2,Node (d,(l2,r2))) )) | a >= e && a < d && b1 && b2 = (True, Node (a,( Node(e,(l1,r1)), Node (d,(l2,r2)))) )
--                                                             | otherwise = (False, Node (a,(Node(e,(l1,r1)),Node (d,(l2,r2)))))


--isOrd = i1 . isOrd' 





--insOrd

--insOrd' x = cataBTree g 
--  where g = either (const (Empty, Node(x,(Empty,Empty)))) insOrd_a x

--insOrd_a x (a,(Empty,Empty)) = if(x <= a) then (Node(a,(Empty,Empty)),Node(a, (Node(x,(Empty,Empty)), Empty))) else (Node(a,(Empty,Empty)),Node(a,(Empty,Node(x,(Empty,Empty)))))


--insOrd_a x (a,(Empty,Node (d,(l1,r2)))) = if(x <= a) then (Node(a,(Empty,Node (d,(l1,r2)))),Node(a, (Node(x,(Empty,Empty)), Node (d,(l1,r2))))) else 

--insOrd a x = p2.insOrd' x

--insereOrdenado :: a -> BTree a -> (BTree a, BTree a)
--insereOrdenado x (Empty) =(Empty, Node(x,(Empty,Empty)))
--insereOrdenado x (a,(Empty,Empty)) | x > a = (Node(a,(Empty,Empty)),Node(a,(Empty,Node(x,(Empty,Empty)))))
--                                   | otherwise = (Node(a,(Empty,Empty)),Node(a,(Node(x,(Empty,Empty)), Empty)))
--insereOrdenado x (a,(e,d)) | x >a = insereOrdenado x d
--                           | otherwise = insereOrdenado x e 


insOrd' x = cataBTree g 
  where g = undefined
  --either (const (Empty, Node(x,(Empty,Empty)))) insOrd_a x


list2BTree []   = Empty
list2BTree [x]  = Node(x, (Empty,Empty))
list2BTree list = Node(x,(list2BTree ltx, list2BTree gtx))
                  where 
                    m = (div (length list) 2)
                    x = list !! m
                    ltx = take m list
                    gtx = drop (m+1) list


insOrd a x = list2BTree .(iSort . inordt)




insOrd' x = cataBTree g 
  where g = either (Empty, Node(x,(Empty,Empty)))  insOrd_a x

--insOrd_a x a = (a, listToBtree . iSort . inordt x ) 

insOrd_a x (a,(_,Empty)) = if(x>a) then (Node(a,(_,Empty)), Node(a,(_,Node(x,(Empty,Empty)))))
insOrd_a x (a,(Empty,_)) = if(x<a) then (Node (a,(Empty,_)), Node (a,(Node(x,(Empty,Empty)),_)))




insOrd a x = p2.insOrd' x






rrot = undefined

lrot = undefined

splay l t =  undefined